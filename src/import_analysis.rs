use std::collections::HashMap;

use crate::parser::*;

// node of the dependency graph
struct ModuleNode {
    dependencies: usize,
    dependents: Vec<String>,
    module: Module
}

// performs a topological sort on a list of modules, returning the sorted list
pub fn order_modules(modules: Vec<Module>) -> Vec<Module> {
    // modules with no further dependencies
    let mut ready: Vec<Module> = Vec::new();
    // dependency graph
    let mut dep_graph: HashMap<String, ModuleNode> = HashMap::new();

    for module in modules {
        dep_graph.insert(
            module.name.clone(),
            ModuleNode {
                dependencies: module.imports.len(),
                dependents: module.imports.clone(),
                module: module
            }
        );
    }

    unimplemented!()
}