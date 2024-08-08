use std::{fs, io};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::sync::Arc;

use crate::syntax::ast::ModuleAst;
use crate::syntax::parser::Parser;

#[derive(Clone)]
enum Module {
    Unread {
        file: Arc<PathBuf>,
        failed_to_read: bool,
    },
    Unparsed {
        file: Arc<PathBuf>,
        contents: Arc<String>,
    },
    Parsed(Arc<ParsedModule>),
}

#[derive(Clone, Debug)]
pub struct ParsedModule {
    pub file: Arc<PathBuf>,
    pub contents: Arc<String>,
    pub ast: Arc<ModuleAst>,
}

impl PartialEq for ParsedModule {
    fn eq(&self, other: &Self) -> bool {
        self.file.eq(&other.file) && self.contents.eq(&other.contents)
    }
}

impl Eq for ParsedModule {}

impl Hash for ParsedModule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.file.hash(state);
        self.contents.hash(state);
    }
}

#[derive(Clone)]
pub struct Modules {
    modules: HashMap<Arc<PathBuf>, Module>,
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn read_and_parse(&mut self, file: Arc<PathBuf>) -> io::Result<Arc<ParsedModule>> {
        let contents = match fs::read_to_string(file.as_path()) {
            Ok(contents) => contents,
            Err(e) => {
                self.modules.insert(
                    file.clone(),
                    Module::Unread {
                        file,
                        failed_to_read: true,
                    },
                );
                return Err(e);
            }
        };

        let parsed = Parser::new(&contents, file.as_path()).parse().unwrap();

        let module = Arc::new(ParsedModule {
            file: file.clone(),
            contents: Arc::new(contents),
            ast: Arc::new(parsed),
        });

        self.modules.insert(file, Module::Parsed(module.clone()));

        Ok(module)
    }
}
