// This should not crash.

let global = $vm.createGlobalObject();
let o = $vm.createProxy(global);
o.__proto__ = o;

try {
    for (let q in o) { }
} catch { }
