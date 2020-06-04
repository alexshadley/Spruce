use std::collections::HashMap;

use crate::parser::*;

// performs a topological sort on a list of modules, returning the sorted list
pub fn order_modules(mods: Vec<Module>) -> Vec<Module> {
    // modules with no further dependencies
    let mut ready: Vec<String> = Vec::new();

    let mut modules: HashMap<String, Module> = HashMap::new();
    let mut dependencies: HashMap<String, usize> = HashMap::new();
    let mut dependents: HashMap<String, Vec<String>> = HashMap::new();

    for module in &mods {
        dependents.insert(module.name.clone(), Vec::new());
    }

    // initialize tables
    for module in mods {
        for dependency in &module.imports {
            let mut mod_deps = dependents.get_mut(dependency).expect("unreachable");
            mod_deps.push(module.name.clone());
        }

        dependencies.insert(
            module.name.clone(),
            module.imports.len()
        );
        modules.insert(
            module.name.clone(),
            module
        );
    }

    // queue modules with 0 dependencies
    for (name, deps) in &dependencies {
        if *deps == 0 {
            ready.push(name.clone());
        }
    }

    let mut ordering: Vec<Module> = Vec::new();

    // main algorithm loop
    while ready.len() > 0 {
        let next_name = ready.pop().expect("unreachable");
        let next_dependents = dependents.get(&next_name).expect("unreachable");

        for dependent in next_dependents {
            let mut dep_count = dependencies.get_mut(dependent).expect("unreachable");
            *dep_count -= 1;
            if *dep_count == 0 {
                ready.push(dependent.clone());
            }
        }

        let module = modules.remove(&next_name).expect("unreachable");
        ordering.push(module);
    }

    ordering
}

#[test]
fn test_ordering() {
    let modules = vec![
        Module {
            name: String::from("A"),
            imports: vec![String::from("B"), String::from("C")],
            functions: vec![],
            interop_functions: vec![],
            definitions: vec![],
            types: vec![]
        },
        Module {
            name: String::from("B"),
            imports: vec![],
            functions: vec![],
            interop_functions: vec![],
            definitions: vec![],
            types: vec![]
        },
        Module {
            name: String::from("C"),
            imports: vec![String::from("B")],
            functions: vec![],
            interop_functions: vec![],
            definitions: vec![],
            types: vec![]
        }
    ];

    let ordering = order_modules(modules);

    assert_eq!(ordering[0].name, String::from("B"));
    assert_eq!(ordering[1].name, String::from("C"));
    assert_eq!(ordering[2].name, String::from("A"));
}
