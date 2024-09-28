
function tesStringStringString(str) {
    return str.replace('h', "abc");
}
noInline(tesStringStringString);

for (let i = 0; i < 1e4; i++) {
    let str = (-1).toLocaleString().padEnd(200, "hello ");
    tesStringStringString(str);
}
