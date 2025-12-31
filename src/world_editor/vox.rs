//! Minimal MagicaVoxel .vox (v150) writer.
//!
//! Teardown uses MagicaVoxel .vox models for most mod content.
//!
//! This writer intentionally supports the subset we need:
//! - one model per file (SIZE + XYZI + RGBA)
//! - up to 255 palette indices (1..=255) (index 0 is reserved)

use std::io::{self, Write};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Rgba {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Rgba {
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VoxVoxel {
    pub x: u8,
    pub y: u8,
    pub z: u8,
    /// Palette index in range 1..=255
    pub i: u8,
}

#[derive(Debug)]
pub struct VoxModel {
    pub size_x: u32,
    pub size_y: u32,
    pub size_z: u32,
    pub voxels: Vec<VoxVoxel>,
    /// 256 RGBA entries. Entry 0 is unused/reserved, but must still be present.
    pub palette: [Rgba; 256],
}

impl VoxModel {
    pub fn validate(&self) -> Result<(), String> {
        if !(1..=256).contains(&self.size_x) || !(1..=256).contains(&self.size_y) || !(1..=256).contains(&self.size_z) {
            return Err(format!(
                "VOX model size must be within 1..=256 per axis, got {}x{}x{}",
                self.size_x, self.size_y, self.size_z
            ));
        }

        for v in &self.voxels {
            if v.x as u32 >= self.size_x || v.y as u32 >= self.size_y || v.z as u32 >= self.size_z {
                return Err("VOX voxel coordinate out of bounds".to_string());
            }
            if v.i == 0 {
                return Err("VOX voxel palette index 0 is reserved".to_string());
            }
        }

        Ok(())
    }
}

pub fn write_vox<W: Write>(mut w: W, model: &VoxModel) -> io::Result<()> {
    // Validate in debug builds to catch mistakes early.
    debug_assert!(model.validate().is_ok());

    // Header
    w.write_all(b"VOX ")?;
    w.write_all(&150u32.to_le_bytes())?;

    // Precompute child chunks size: SIZE + XYZI + RGBA
    let size_chunk_len = 12 + 12; // header+content
    let xyzi_chunk_len = 12 + 4 + model.voxels.len() * 4;
    let rgba_chunk_len = 12 + 256 * 4;
    let children_size = (size_chunk_len + xyzi_chunk_len + rgba_chunk_len) as u32;

    // MAIN chunk header
    w.write_all(b"MAIN")?;
    w.write_all(&0u32.to_le_bytes())?; // no content
    w.write_all(&children_size.to_le_bytes())?;

    // SIZE chunk
    w.write_all(b"SIZE")?;
    w.write_all(&12u32.to_le_bytes())?;
    w.write_all(&0u32.to_le_bytes())?;
    w.write_all(&(model.size_x as u32).to_le_bytes())?;
    w.write_all(&(model.size_y as u32).to_le_bytes())?;
    w.write_all(&(model.size_z as u32).to_le_bytes())?;

    // XYZI chunk
    w.write_all(b"XYZI")?;
    let xyzi_content_size = (4 + model.voxels.len() * 4) as u32;
    w.write_all(&xyzi_content_size.to_le_bytes())?;
    w.write_all(&0u32.to_le_bytes())?;
    w.write_all(&(model.voxels.len() as u32).to_le_bytes())?;
    for v in &model.voxels {
        w.write_all(&[v.x, v.y, v.z, v.i])?;
    }

    // RGBA chunk
    w.write_all(b"RGBA")?;
    w.write_all(&(256u32 * 4).to_le_bytes())?;
    w.write_all(&0u32.to_le_bytes())?;
    for c in &model.palette {
        w.write_all(&[c.r, c.g, c.b, c.a])?;
    }

    Ok(())
}
