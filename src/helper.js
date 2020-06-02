// Helper functions for Spruce codegen

function printInt(i) {
    console.log(i);
}

function _to_bool(b) {
    if (b) {
        return [Bool.TRUE]
    }
    else {
        return [Bool.FALSE]
    }
}

function _push_and_copy(list, val) {
    var new_list = list.map((x) => x);
    new_list.push(val);
    return new_list;
}
