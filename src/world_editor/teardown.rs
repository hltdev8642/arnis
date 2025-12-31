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
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::Write;
use std::path::Path;

const TILE_SIZE_X: i32 = 128;
const TILE_SIZE_Y: i32 = 128;
const TILE_SIZE_Z: i32 = 128;

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
    // Note: Teardown editor may add more nodes; we only need to reference the vox bodies.
    let mut xml = String::new();
    xml.push_str("<scene>\n");
    xml.push_str("  <environment template=\"sunny\"/>\n");
    xml.push_str("  <player pos=\"0 10 0\" rot=\"0 0 0\"/>\n");
    xml.push_str(&format!("  <group name=\"{}\">\n", mod_name));

    for (i, vf) in vox_files.iter().enumerate() {
        xml.push_str(&format!(
            "    <body name=\"tile_{i}\" pos=\"0 0 0\" dynamic=\"false\">\n"
        ));
        xml.push_str(&format!("      <vox file=\"MOD/vox/{vf}\"/>\n"));
        xml.push_str("    </body>\n");
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
        let vox_dir = self.world_dir.join("vox");
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

        let (block_to_palette, palette, fallback_index) = build_palette_index_for_blocks(self);

        // Collect voxels per tile.
        // BTreeMap for deterministic output ordering.
        let mut tiles: BTreeMap<TileKey, Vec<VoxVoxel>> = BTreeMap::new();

        for (&(region_x, region_z), region) in &self.world.regions {
            iter_blocks_in_region(region_x, region_z, region, |x, y, z, block| {
                if block == AIR || block.name() == "water" {
                    return;
                }

                let (tx, lx) = tile_index(x, origin_x, TILE_SIZE_X);
                let (ty, ly) = tile_index(y, origin_y, TILE_SIZE_Y);
                let (tz, lz) = tile_index(z, origin_z, TILE_SIZE_Z);

                let i = block_to_palette
                    .get(&block.id())
                    .copied()
                    .unwrap_or(fallback_index);

                let key = TileKey { tx, ty, tz };
                tiles.entry(key).or_default().push(VoxVoxel {
                    x: lx,
                    y: ly,
                    z: lz,
                    i,
                });
            });
        }

        // Write each tile to its own .vox file.
        let mut vox_filenames: Vec<String> = Vec::with_capacity(tiles.len());

        let total_tiles = tiles.len().max(1);
        let mut tile_idx = 0usize;
        for (key, voxels) in tiles {
            tile_idx += 1;
            emit_gui_progress_update(90.0 + 9.0 * (tile_idx as f64 / total_tiles as f64), "Writing vox tiles...");

            let filename = format!("terrain_{}_{}_{}.vox", key.tx, key.ty, key.tz);
            let out_path = vox_dir.join(&filename);

            let model = VoxModel {
                size_x: TILE_SIZE_X as u32,
                size_y: TILE_SIZE_Y as u32,
                size_z: TILE_SIZE_Z as u32,
                voxels,
                palette,
            };

            // Validate in all builds (we want a good error, not a silent corrupt file)
            model.validate().map_err(|e| {
                format!(
                    "Invalid .vox model for tile ({},{},{}): {e}",
                    key.tx, key.ty, key.tz
                )
            })?;

            let mut f = fs::File::create(&out_path)
                .map_err(|e| format!("Failed to create {}: {e}", out_path.display()))?;
            write_vox(&mut f, &model)
                .map_err(|e| format!("Failed to write {}: {e}", out_path.display()))?;
            f.flush()
                .map_err(|e| format!("Failed to flush {}: {e}", out_path.display()))?;

            vox_filenames.push(filename);
        }

        write_main_xml(&self.world_dir, &mod_name, &vox_filenames)?;

        Ok(())
    }
}
