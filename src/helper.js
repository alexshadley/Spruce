// Helper functions for Spruce codegen

function _to_bool(b) {
    if (b) {
        return [Bool.TRUE]
    }
    else {
        return [Bool.FALSE]
    }
}
