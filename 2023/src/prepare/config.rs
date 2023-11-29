use std::{default, io, process};

use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub session: String,
}

impl default::Default for Config {
    fn default() -> Self {
        Self {
            session: ask_session(),
        }
    }
}

pub fn load_config() -> Result<Config> {
    let app_name = env!("CARGO_PKG_NAME");
    let config: Config = confy::load(app_name, None)?;
    let file = confy::get_configuration_file_path(app_name, None)?;
    println!("Using configuration file: {}", file.display());

    Ok(config)
}

fn ask_session() -> String {
    let mut session = String::new();
    print!("No configuration file found. Enter session: ");

    if let Err(e) = io::stdin().read_line(&mut session) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }

    session
}
