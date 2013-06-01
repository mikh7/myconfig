var dirService = Components.classes["@mozilla.org/file/directory_service;1"].getService(Components.interfaces.nsIProperties); 

var homeDirFile = dirService.get("Home", Components.interfaces.nsIFile);

function expandFileName(name) {
    var file = Components.classes["@mozilla.org/file/local;1"].
        createInstance(Components.interfaces.nsILocalFile);
    file.initWithPath(name);
    return file;
}

function loadFile(name) {
    try {
        name = expandFileName(name);
        var loader = Components.classes["@mozilla.org/moz/jssubscript-loader;1"].getService(Components.interfaces.mozIJSSubScriptLoader);
        loader.loadSubScript('file://' + name.path);
    } catch (e) {
        alert('Got error: ' + e);
    }
}



loadFile('~/myconfig/firefox/logging.js');

function loadFile(name) {
    try {
        name = expandFileName(name);
        var loader = Components.classes["@mozilla.org/moz/jssubscript-loader;1"].getService(Components.interfaces.mozIJSSubScriptLoader);
        loader.loadSubScript('file://' + name.path);
    } catch (e) {
        mDebug.print('While loading ' + name.path + ' got error ' + e);
        mDebug.printStack();
        throw(e);
    }
}

dactyl.loadFile = loadFile;

function readFileToString(name) {
    var file = Components.classes["@mozilla.org/file/local;1"]
        .createInstance(Components.interfaces.nsILocalFile);

    var fstream = Components.classes["@mozilla.org/network/file-input-stream;1"]
        .createInstance(Components.interfaces.nsIFileInputStream);
    var sstream = Components.classes["@mozilla.org/scriptableinputstream;1"]
        .createInstance(Components.interfaces.nsIScriptableInputStream);

    file.initWithPath(name);

    fstream.init(file, 0x01, 00004, null);
    sstream.init(fstream);

    var output = sstream.read(sstream.available());
    sstream.close();
    fstream.close();
    return output;
}

function writeStringToFile(name, data) {
    var file = Components.classes["@mozilla.org/file/local;1"]
        .createInstance(Components.interfaces.nsILocalFile);
    var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"].
        createInstance(Components.interfaces.nsIFileOutputStream);

    file.initWithPath(name);

    // use 0x02 | 0x10 to open file for appending.
    foStream.init(file, 0x02 | 0x08 | 0x20, 0666, 0); 
    // write, create, truncate
    // In a c file operation, we have no need to set file mode with or operation,
    // directly using "r" or "w" usually.

    // if you are sure there will never ever be any non-ascii text in data you can 
    // also call foStream.writeData directly
    var converter = Components.classes["@mozilla.org/intl/converter-output-stream;1"].
        createInstance(Components.interfaces.nsIConverterOutputStream);
    converter.init(foStream, "UTF-8", 0, 0);
    converter.writeString(data);
    converter.close(); // this closes foStream
}

dactyl.goFireBugConsole = function() {
    Firebug.toggleBar(true, 'console')
    Firebug.CommandLine.focus(Firebug.currentContext)
}
