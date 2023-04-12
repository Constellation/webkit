(function() {
    var global = $vm.createGlobalObject();
    Object.defineProperty(global, 'foo', {
        set(_v) { }
    });
    var proxy = $vm.createProxy(global);

    for (var j = 0; j < 1e5; j++)
        proxy["foo" + j] = j;
})();
