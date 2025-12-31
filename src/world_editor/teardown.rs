//! Teardown content-mod export.
//!
//! This exporter writes a Teardown *content mod* folder structure:
//! - info.txt
//! - main.xml
//! - vox/*.vox (MagicaVoxel v150 files)

use super::common::{RegionToModify, SectionToModify};
use super::vox::{write_vox, Rgba, VoxModel, VoxVoxel};
use super::WorldEditor;
use crate::block_definitions::AIR;
use crate::map_renderer::block_name_to_rgb;
use crate::progress::emit_gui_progress_update;
use colored::Colorize;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs::{self, File};
use std::io::{Write, Read, BufWriter};
use std::path::{Path, PathBuf};

// Scale calibration reference:
// - The vanilla station wagon VOX has a primary model SIZE of 47x23x13 voxels.
// - The stationwagon prefab places lights at z=-2.3 and z=+2.1 (≈4.4 meters total length).
// => ~10.68 voxels per meter, rounded to 11.
const REF_STATIONWAGON_LENGTH_M: f64 = 4.4;
const REF_STATIONWAGON_LENGTH_VOXELS: f64 = 47.0;

fn calibrated_voxels_per_meter() -> f64 {
    REF_STATIONWAGON_LENGTH_VOXELS / REF_STATIONWAGON_LENGTH_M
}

fn meters_per_block(blocks_per_meter: f64) -> f64 {
    if blocks_per_meter.is_finite() && blocks_per_meter > 0.0 {
        1.0 / blocks_per_meter
    } else {
        1.0
    }
}

fn recommended_blocks_per_meter_for_stationwagon_scale() -> f64 {
    // If we export with 1 voxel per generated block (best for Teardown collision),
    // then the generation density should be approximately "voxels per meter".
    calibrated_voxels_per_meter()
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct TileKey {
    tx: i32,
    ty: i32,
    tz: i32,
}

#[derive(Debug)]
struct Bounds {
    min_x: i32,
    max_x: i32,
    min_y: i32,
    max_y: i32,
    min_z: i32,
    max_z: i32,
}

impl Bounds {
    fn new() -> Self {
        Self {
            min_x: i32::MAX,
            max_x: i32::MIN,
            min_y: i32::MAX,
            max_y: i32::MIN,
            min_z: i32::MAX,
            max_z: i32::MIN,
        }
    }

    fn include(&mut self, x: i32, y: i32, z: i32) {
        self.min_x = self.min_x.min(x);
        self.max_x = self.max_x.max(x);
        self.min_y = self.min_y.min(y);
        self.max_y = self.max_y.max(y);
        self.min_z = self.min_z.min(z);
        self.max_z = self.max_z.max(z);
    }

    fn is_valid(&self) -> bool {
        self.min_x <= self.max_x && self.min_y <= self.max_y && self.min_z <= self.max_z
    }
}

fn tile_index(value: i32, origin: i32, tile_size: i32) -> (i32, u8) {
    let rel = value - origin;
    let t = rel.div_euclid(tile_size);
    let local = rel.rem_euclid(tile_size);
    // tile_size is <= 256, safe
    (t, local as u8)
}

fn default_palette() -> [Rgba; 256] {
    // Create a pleasant default palette (index 0 unused).
    // Colors are overridden per block as we assign indices.
    let mut p = [Rgba::new(255, 255, 255, 255); 256];
    p[0] = Rgba::new(0, 0, 0, 0);

    // Fill with a deterministic gradient so unassigned indices don't look identical.
    for i in 1..256u16 {
        let v = i as u8;
        p[i as usize] = Rgba::new(v, v, v, 255);
    }

    p
}

fn sanitize_mod_name(name: &str) -> String {
    // Teardown docs recommend latin alphanumeric + space.
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == ' ' {
            out.push(ch);
        }
    }
    let out = out.trim();
    if out.is_empty() {
        "Arnis Teardown Map".to_string()
    } else {
        out.to_string()
    }
}

fn write_info_txt(mod_root: &Path, name: &str, description: &str) -> Result<(), String> {
    let info_path = mod_root.join("info.txt");
    let contents = format!(
        "name = {name}\nauthor = Arnis\ndescription = {description}\ntags = Map\n"
    );
    fs::write(&info_path, contents)
        .map_err(|e| format!("Failed to write {}: {e}", info_path.display()))
}

fn write_main_lua(mod_root: &Path) -> Result<(), String> {
    // Not required, but harmless and useful for future extension.
    let lua_path = mod_root.join("main.lua");
    if lua_path.exists() {
        return Ok(());
    }

    fs::write(
        &lua_path,
        "function init()\nend\n\nfunction tick(dt)\nend\n",
    )
    .map_err(|e| format!("Failed to write {}: {e}", lua_path.display()))
}

fn write_main_xml(mod_root: &Path, mod_name: &str, vox_files: &[String]) -> Result<(), String> {
    let xml_path = mod_root.join("main.xml");

    // Minimal-but-playable scene.
    // The caller provides a list of voxel tiles and their placement offsets.
    let mut xml = String::new();
    xml.push_str("<scene>\n");
    xml.push_str("  <environment template=\"sunny\"/>\n");
    xml.push_str("  <player pos=\"0 10 0\" rot=\"0 0 0\"/>\n");
    xml.push_str(&format!("  <group name=\"{}\">\n", mod_name));

    for vf in vox_files {
        // This function expects entries formatted as: "filename|x|y|z".
        // (We keep the signature simple and build these strings in the exporter loop.)
        let mut parts = vf.split('|');
        let filename = parts.next().unwrap_or(vf);
        let x = parts.next().and_then(|p| p.parse::<f64>().ok()).unwrap_or(0.0);
        let y = parts.next().and_then(|p| p.parse::<f64>().ok()).unwrap_or(0.0);
        let z = parts.next().and_then(|p| p.parse::<f64>().ok()).unwrap_or(0.0);

        // Teardown places the VOX at a world position; we rotate to match expected axis orientation.
        xml.push_str(&format!(
            "    <vox pos=\"{x:.3} {y:.3} {z:.3}\" rot=\"90 0 0\" file=\"MOD/output/vox/{filename}\"/>\n"
        ));
    }

    xml.push_str("  </group>\n");
    xml.push_str("</scene>\n");

    fs::write(&xml_path, xml)
        .map_err(|e| format!("Failed to write {}: {e}", xml_path.display()))
}

fn iter_blocks_in_region(
    region_x: i32,
    region_z: i32,
    region: &RegionToModify,
    mut f: impl FnMut(i32, i32, i32, crate::block_definitions::Block),
) {
    for (&(chunk_x, chunk_z), chunk) in &region.chunks {
        let abs_chunk_x = (region_x << 5) + chunk_x;
        let abs_chunk_z = (region_z << 5) + chunk_z;

        for (&section_y, section) in &chunk.sections {
            let base_y = (section_y as i32) << 4;
            iter_blocks_in_section(abs_chunk_x, abs_chunk_z, base_y, section, &mut f);
        }
    }
}

fn iter_blocks_in_section(
    abs_chunk_x: i32,
    abs_chunk_z: i32,
    base_y: i32,
    section: &SectionToModify,
    f: &mut impl FnMut(i32, i32, i32, crate::block_definitions::Block),
) {
    for y in 0..16u8 {
        for z in 0..16u8 {
            for x in 0..16u8 {
                let idx = SectionToModify::index(x, y, z);
                let b = section.blocks[idx];
                if b == AIR {
                    continue;
                }

                let wx = (abs_chunk_x << 4) + x as i32;
                let wy = base_y + y as i32;
                let wz = (abs_chunk_z << 4) + z as i32;
                f(wx, wy, wz, b);
            }
        }
    }
}

fn compute_bounds(editor: &WorldEditor) -> Option<Bounds> {
    let mut bounds = Bounds::new();

    for (&(region_x, region_z), region) in &editor.world.regions {
        iter_blocks_in_region(region_x, region_z, region, |x, y, z, block| {
            // Skip water blocks: Teardown water should be authored as water nodes, not voxel solids.
            if block.name() == "water" {
                return;
            }
            bounds.include(x, y, z);
        });
    }

    bounds.is_valid().then_some(bounds)
}

fn build_palette_index_for_blocks(
    editor: &WorldEditor,
) -> (HashMap<u8, u8>, [Rgba; 256], u8) {
    // block.id() -> palette index
    let mut mapping: HashMap<u8, u8> = HashMap::new();
    let mut palette = default_palette();

    // Reserve index 1 for unknown blocks.
    palette[1] = Rgba::new(160, 160, 160, 255);
    let mut next_index: u8 = 2;

    for (&(region_x, region_z), region) in &editor.world.regions {
        iter_blocks_in_region(region_x, region_z, region, |_, _, _, block| {
            if block == AIR || block.name() == "water" {
                return;
            }

            let block_id = block.id();
            if mapping.contains_key(&block_id) {
                return;
            }

            if next_index == 0 || next_index >= 255 {
                // Palette exhausted; use index 1.
                mapping.insert(block_id, 1);
                return;
            }

            let (r, g, b) = block_name_to_rgb(block.name());
            palette[next_index as usize] = Rgba::new(r, g, b, 255);
            mapping.insert(block_id, next_index);
            next_index = next_index.saturating_add(1);
        });
    }

    (mapping, palette, 1)
}

impl<'a> WorldEditor<'a> {
    /// Flushes accumulated regions from memory for Teardown.
    /// Since Teardown export processes all regions during the final export phase,
    /// we can safely clear them from memory after they're no longer needed.
    /// The final export will need the regions, so this should only be called
    /// during intermediate phases if absolutely necessary.
    pub(super) fn flush_teardown_regions(&mut self) -> Result<(), String> {
        // For Teardown: Simply clear regions from memory to prevent OOM.
        // This will discard ground blocks, but it's better than crashing.
        // The export phase will only export whatever blocks remain after flushes.
        
        let region_count = self.world.regions.len();
        if region_count == 0 {
            return Ok(());
        }
        
        self.world.regions.clear();
        println!("  Teardown flush: cleared {} regions from memory (ground blocks discarded)", region_count);
        Ok(())
    }

    pub(super) fn save_teardown(&mut self) {
        println!("{} Saving Teardown content mod...", "[7/7]".bold());
        emit_gui_progress_update(90.0, "Saving Teardown content mod...");

        if let Err(e) = self.save_teardown_internal() {
            eprintln!("Failed to save Teardown mod: {e}");
        }
    }

    fn save_teardown_internal(&mut self) -> Result<(), String> {
        fs::create_dir_all(&self.world_dir)
            .map_err(|e| format!("Failed to create output folder {}: {e}", self.world_dir.display()))?;
        let vox_dir = self.world_dir.join("output").join("vox");
        fs::create_dir_all(&vox_dir)
            .map_err(|e| format!("Failed to create vox folder {}: {e}", vox_dir.display()))?;

        let mod_name = sanitize_mod_name(
            self.world_dir
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("Arnis Teardown Map"),
        );

        write_info_txt(&self.world_dir, &mod_name, "Generated environment from OpenStreetMap data")?;
        write_main_lua(&self.world_dir)?;

        let bounds = compute_bounds(self).ok_or_else(|| "World contains no blocks to export".to_string())?;
        let origin_x = bounds.min_x;
        let origin_y = bounds.min_y;
        let origin_z = bounds.min_z;

        // IMPORTANT: Keep voxel scale at 1 (1 generated block -> 1 voxel).
        // Scaling by voxel-duplication (vpb>1) explodes voxel counts and can crash Teardown.
        let vpb: u8 = 1;
        // Restore tile size to 256 - memory is managed via disk streaming now
        let tile_size_x: i32 = 256;
        let tile_size_y: i32 = 256;
        let tile_size_z: i32 = 256;

        let mpb = meters_per_block(self.blocks_per_meter());

        println!(
            "Teardown export: 1 voxel/block, {:.3} m/block, tile {}x{}x{} blocks. Stationwagon scale suggests --scale ~{:.2} (blocks/m)",
            mpb,
            tile_size_x,
            tile_size_y,
            tile_size_z,
            recommended_blocks_per_meter_for_stationwagon_scale()
        );

        let (block_to_palette, palette, fallback_index) = build_palette_index_for_blocks(self);

        // Create temp directory for streaming voxels to disk
        let temp_dir = self.world_dir.join("output").join(".temp_voxels");
        fs::create_dir_all(&temp_dir)
            .map_err(|e| format!("Failed to create temp folder {}: {}", temp_dir.display(), e))?;
        
        println!("Streaming voxels to disk: {}", temp_dir.display());

        // Track which tiles we're actively writing to (on disk)
        let mut active_tile_files: HashMap<TileKey, BufWriter<File>> = HashMap::new();
        let mut tiles_seen: HashSet<TileKey> = HashSet::new();
        let mut vox_placements: Vec<String> = Vec::new();
        let mut tiles_written = 0usize;
        
        // Helper closure to read voxels from temp file and write final VOX
        let write_tile_from_disk = |key: &TileKey, temp_dir: &Path, vox_dir: &Path, palette: &[Rgba; 256], mpb: f64, tile_size_x: i32, tile_size_y: i32, tile_size_z: i32, vpb: u8| -> Result<String, String> {
            let temp_path = temp_dir.join(format!("tile_{}_{}_{}. bin", key.tx, key.ty, key.tz));
            
            // Read voxels from temp file
            let mut voxels = Vec::new();
            if temp_path.exists() {
                let mut temp_file = File::open(&temp_path)
                    .map_err(|e| format!("Failed to open temp file {}: {}", temp_path.display(), e))?;
                
                // Each voxel is 4 bytes: x, y, z, i
                let mut buf = [0u8; 4];
                while temp_file.read_exact(&mut buf).is_ok() {
                    voxels.push(VoxVoxel {
                        x: buf[0],
                        y: buf[1],
                        z: buf[2],
                        i: buf[3],
                    });
                }
            }
            
            if voxels.is_empty() {
                return Ok(String::new()); // Skip empty tiles
            }

            let filename = format!("terrain_{}_{}_{}. vox", key.tx, key.ty, key.tz);
            let out_path = vox_dir.join(&filename);

            let model = VoxModel {
                size_x: (tile_size_x as u32) * (vpb as u32),
                size_y: (tile_size_y as u32) * (vpb as u32),
                size_z: (tile_size_z as u32) * (vpb as u32),
                voxels,
                palette: *palette,
            };

            model.validate().map_err(|e| {
                format!("Invalid .vox model for tile ({},{},{}): {}", key.tx, key.ty, key.tz, e)
            })?;

            let mut f = File::create(&out_path)
                .map_err(|e| format!("Failed to create {}: {}", out_path.display(), e))?;
            write_vox(&mut f, &model)
                .map_err(|e| format!("Failed to write {}: {}", out_path.display(), e))?;
            f.flush()
                .map_err(|e| format!("Failed to flush {}: {}", out_path.display(), e))?;

            // Clean up temp file
            let _ = fs::remove_file(&temp_path);

            let px = (key.tx as f64) * (tile_size_x as f64) * mpb;
            let py = (key.ty as f64) * (tile_size_y as f64) * mpb;
            let pz = (key.tz as f64) * (tile_size_z as f64) * mpb;
            Ok(format!("{filename}|{px}|{py}|{pz}"))
        };

        let total_regions = self.world.regions.len().max(1);
        
        // Helper closure to flush tiles from disk
        let flush_tiles_from_disk = |tiles_to_flush: &[TileKey], 
                                      temp_dir: &Path,
                                      vox_dir: &Path,
                                      palette: &[Rgba; 256],
                                      vox_placements: &mut Vec<String>,
                                      tiles_written: &mut usize,
                                      tile_size_x: i32,
                                      tile_size_y: i32,
                                      tile_size_z: i32,
                                      vpb: u8,
                                      mpb: f64| -> Result<(), String> {
            for key in tiles_to_flush {
                match write_tile_from_disk(key, temp_dir, vox_dir, palette, mpb, tile_size_x, tile_size_y, tile_size_z, vpb) {
                    Ok(placement) if !placement.is_empty() => {
                        vox_placements.push(placement);
                        *tiles_written += 1;
                    }
                    Ok(_) => {}, // Empty tile, skip
                    Err(e) => return Err(e),
                }
            }
            Ok(())
        };

        let mut chunks_processed = 0usize;
        let mut total_chunks = 0usize;
        for region in self.world.regions.values() {
            total_chunks += region.chunks.len();
        }
        
        println!("Processing {} chunks across {} regions (disk-streaming mode)...", total_chunks, total_regions);

        for (&(region_x, region_z), region) in &self.world.regions {
            // Process CHUNK BY CHUNK, writing voxels directly to disk temp files
            for (chunk_key, chunk) in &region.chunks {
                for (section_y, section) in &chunk.sections {
                    let section_base_y = (*section_y as i32) * 16;
                    let chunk_x = chunk_key.0;
                    let chunk_z = chunk_key.1;
                    
                    for local_y in 0..16 {
                        for local_z in 0..16 {
                            for local_x in 0..16 {
                                let idx = (local_y * 16 * 16 + local_z * 16 + local_x) as usize;
                                if idx >= section.blocks.len() {
                                    continue;
                                }
                                let block = &section.blocks[idx];
                                
                                if *block == AIR || block.name() == "water" {
                                    continue;
                                }

                                let x = region_x * 512 + chunk_x * 16 + local_x;
                                let y = section_base_y + (local_y as i32);
                                let z = region_z * 512 + chunk_z * 16 + local_z;

                                let (tx, lx) = tile_index(x, origin_x, tile_size_x);
                                let (ty, ly) = tile_index(y, origin_y, tile_size_y);
                                let (tz, lz) = tile_index(z, origin_z, tile_size_z);

                                let i = block_to_palette
                                    .get(&block.id())
                                    .copied()
                                    .unwrap_or(fallback_index);

                                let key = TileKey { tx, ty, tz };
                                
                                // Track that we've seen this tile
                                tiles_seen.insert(key);

                                let base_x: u16 = (lx as u16) * (vpb as u16);
                                let base_y: u16 = (ly as u16) * (vpb as u16);
                                let base_z: u16 = (lz as u16) * (vpb as u16);

                                // Get or create buffered writer for this tile
                                let writer = active_tile_files.entry(key).or_insert_with(|| {
                                    let temp_path = temp_dir.join(format!("tile_{}_{}_{}. bin", key.tx, key.ty, key.tz));
                                    BufWriter::new(File::create(&temp_path).expect("Failed to create temp file "))
                                });
                                
                                // Write voxel as 4 bytes directly to disk
                                let voxel_bytes = [base_x as u8, base_y as u8, base_z as u8, i];
                                writer.write_all(&voxel_bytes).map_err(|e| format!("Failed to write voxel: {}", e))?;
                            }
                        }
                    }
                }
                
                // Flush file buffers after each chunk to ensure data is written
                for writer in active_tile_files.values_mut() {
                    writer.flush().map_err(|e| format!("Failed to flush temp file: {}", e))?;
                }
                
                chunks_processed += 1;
                if chunks_processed % 100 == 0 {
                    println!("  Processed {}/{} chunks, {} unique tiles on disk...", chunks_processed, total_chunks, tiles_seen.len());
                    emit_gui_progress_update(90.0 + 8.0 * (chunks_processed as f64 / total_chunks as f64), "Streaming voxels to disk...");
                }
            }
        }
        
        // Close all temp file writers
        println!("Closing temp files...");
        active_tile_files.clear();

        // Now read all tiles from disk and write final VOX files
        println!("Writing {} VOX tiles from disk...", tiles_seen.len());
        let tiles_to_flush: Vec<TileKey> = tiles_seen.iter().copied().collect();
        flush_tiles_from_disk(&tiles_to_flush, &temp_dir, &vox_dir, &palette, &mut vox_placements, &mut tiles_written, tile_size_x, tile_size_y, tile_size_z, vpb, mpb)?;

        // Clean up temp directory
        let _ = fs::remove_dir_all(&temp_dir);

        println!("Wrote {} VOX tiles (disk-streaming mode) ", tiles_written);

        write_main_xml(&self.world_dir, &mod_name, &vox_placements)?;

        Ok(())
    }
}
