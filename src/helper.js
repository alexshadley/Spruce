// Helper functions for Spruce codegen

function _to_bool(b) {
    if (b) {
        return [Bool.TRUE]
    }
    else {
        return [Bool.FALSE]
    }
}

// from: https://medium.com/javascript-in-plain-english/how-to-deep-copy-objects-and-arrays-in-javascript-7c911359b089
function _deep_copy(obj) {
    if (typeof obj !== "object" || obj === null) {
      return obj; // Return the value if inObject is not an object
    }
  
    // Create an array or object to hold the values
    let out = Array.isArray(obj) ? [] : {};
  
    for (let key in obj) {
      let value = obj[key];
  
      // Recursively (deep) copy for nested objects, including arrays
      out[key] = _deep_copy(value);
    }
  
    return out;
}

function _push_and_copy(list, val) {
    var new_list = _deep_copy(list);
    new_list.push(val);
    return new_list;
}

function _set_and_copy(dict, key, val) {
    var new_dict = _deep_copy(dict)
    new_dict[key] = val;
    return new_dict;
}
