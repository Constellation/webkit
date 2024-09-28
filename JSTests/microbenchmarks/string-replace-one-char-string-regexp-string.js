
function tesStringRegExpString(str) {
    return str.replace(/h/, "abc");
}
noInline(tesStringRegExpString);

for (let i = 0; i < 1e4; i++) {
    let str = (-1).toLocaleString().padEnd(200, "hello ");
    tesStringRegExpString(str);
}
