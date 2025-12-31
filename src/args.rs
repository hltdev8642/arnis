use crate::coordinate_system::geographic::LLBBox;
use clap::Parser;
use clap::ValueEnum;
use std::path::PathBuf;
use std::time::Duration;

#[derive(ValueEnum, Debug, Clone, Copy, Eq, PartialEq)]
pub enum OutputFormat {
    /// Export a Teardown content-mod folder (info.txt + main.xml + vox/*)
    Teardown,
    /// Export a Minecraft Java Edition world (Anvil region files)
    MinecraftJava,
    /// Export a Minecraft Bedrock Edition world (.mcworld) (requires the `bedrock` feature)
    MinecraftBedrock,
}

/// Command-line arguments parser
#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Args {
    /// Bounding box of the area (min_lat,min_lng,max_lat,max_lng) (required)
    #[arg(long, allow_hyphen_values = true, value_parser = LLBBox::from_str)]
    pub bbox: LLBBox,

    /// JSON file containing OSM data (optional)
    #[arg(long, group = "location")]
    pub file: Option<String>,

    /// JSON file to save OSM data to (optional)
    #[arg(long, group = "location")]
    pub save_json_file: Option<String>,

    /// Output directory.
    ///
    /// - `--format teardown`: This is the Teardown mod folder to create/write (will be created if missing)
    /// - `--format minecraft-java`: This must point to an existing Minecraft world folder containing `region/`
    /// - `--format minecraft-bedrock`: Output path for the `.mcworld` file
    #[arg(long)]
    pub path: PathBuf,

    /// Export target format
    #[arg(long, value_enum, default_value_t = OutputFormat::Teardown)]
    pub format: OutputFormat,

    /// Downloader method (requests/curl/wget) (optional)
    #[arg(long, default_value = "requests")]
    pub downloader: String,

    /// World scale to use, in blocks per meter
    #[arg(long, default_value_t = 1.0)]
    pub scale: f64,

    /// Ground level to use in the Minecraft world
    #[arg(long, default_value_t = -62)]
    pub ground_level: i32,

    /// Enable terrain (optional)
    #[arg(long)]
    pub terrain: bool,

    /// Enable interior generation (optional)
    #[arg(long, default_value_t = true, action = clap::ArgAction::SetTrue)]
    pub interior: bool,

    /// Enable roof generation (optional)
    #[arg(long, default_value_t = true, action = clap::ArgAction::SetTrue)]
    pub roof: bool,

    /// Enable filling ground (optional)
    #[arg(long, default_value_t = false, action = clap::ArgAction::SetFalse)]
    pub fillground: bool,

    /// Enable debug mode (optional)
    #[arg(long)]
    pub debug: bool,

    /// Set floodfill timeout (seconds) (optional)
    #[arg(long, value_parser = parse_duration)]
    pub timeout: Option<Duration>,

    /// Spawn point coordinates (lat, lng)
    #[arg(skip)]
    pub spawn_point: Option<(f64, f64)>,
}

fn parse_duration(arg: &str) -> Result<std::time::Duration, std::num::ParseIntError> {
    let seconds = arg.parse()?;
    Ok(std::time::Duration::from_secs(seconds))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmpdir() -> tempfile::TempDir {
        tempfile::tempdir().unwrap()
    }
    #[test]
    fn test_flags() {
        let tmpdir = tmpdir();
        let tmp_path = tmpdir.path().to_str().unwrap();

        // Test that terrain/debug are SetTrue
        let cmd = ["arnis", "--path", tmp_path, "--bbox", "1,2,3,4", "--terrain", "--debug"];
        let args = Args::parse_from(cmd.iter());
        assert!(args.debug);
        assert!(args.terrain);

        let cmd = ["arnis", "--path", tmp_path, "--bbox", "1,2,3,4"];
        let args = Args::parse_from(cmd.iter());
        assert!(!args.debug);
        assert!(!args.terrain);
    }

    #[test]
    fn test_required_options() {
        let tmpdir = tmpdir();
        let tmp_path = tmpdir.path().to_str().unwrap();

        let cmd = ["arnis"];
        assert!(Args::try_parse_from(cmd.iter()).is_err());

        let cmd = ["arnis", "--path", tmp_path, "--bbox", "1,2,3,4"];
        assert!(Args::try_parse_from(cmd.iter()).is_ok());

        let cmd = ["arnis", "--path", tmp_path, "--file", ""];
        assert!(Args::try_parse_from(cmd.iter()).is_err());

        // The --gui flag isn't used here, ugh. TODO clean up main.rs and its argparse usage.
        // let cmd = ["arnis", "--gui"];
        // assert!(Args::try_parse_from(cmd.iter()).is_ok());
    }

    #[test]
    fn test_format_parses() {
        let tmpdir = tmpdir();
        let tmp_path = tmpdir.path().to_str().unwrap();

        let cmd = [
            "arnis",
            "--path",
            tmp_path,
            "--bbox",
            "1,2,3,4",
            "--format",
            "minecraft-java",
        ];

        let args = Args::parse_from(cmd.iter());
        assert_eq!(args.format, OutputFormat::MinecraftJava);
    }
}
