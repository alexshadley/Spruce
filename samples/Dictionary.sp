main() {
    mut myDict = dictEmpty()
    myDict := dictSet(myDict, "towels", 5)
    myDict := dictSet(myDict, "tea", 42)

    console.log(dictGet(myDict, "tea"))
    console.log(dictGet(myDict, "fish"))
}
