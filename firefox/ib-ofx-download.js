
/**
 * Inject a function called loadScript() into the specified window
 * its frames, that will load a local .js file into the document
 */
function setupLoadScript(window) {
    if ('wrappedJSObject' in window)
        window = window.wrappedJSObject;
    window.loadScript = function (name) {
        mDebug.print('Will attempt to load ' + name + ' into the document');
        var data = readFileToString(name);
        var document = window.document;
        var s = document.createElement('script');
        s.type = 'text/javascript';
        s.appendChild(document.createTextNode(data));
        try {
            document.head.appendChild(s);
        } catch (e) {
            mDebug.print("Caught error " + e);
        }
    };
    var i;
    // mDebug.print("Setting up frames.length=" + window.frames.length);
    for (i = 0; i < window.frames.length; i++) {
        var f = window.frames[i];
        setupLoadScript(f);
    }
}

setupLoadScript(content.window);

function isDownloadActivityPage () {
    var htmlWindow = content.document.getElementById('content').contentWindow.wrappedJSObject;
    if (htmlWindow.document.getElementById('dat'))
        return true;
    return false;
}

function getOfxFileDirectory() {
    var dir = prompt("Directory to store OFX files in", "~/ib-ofx")
    if (dir != null) {
        dir = expandFileName(dir);
        if( !dir.exists() || !dir.isDirectory() ) {   // if it doesn't exist, create
            dir.create(Components.interfaces.nsIFile.DIRECTORY_TYPE, 0777);
        }
        return dir;
    }
    return null
}

function writeOfxFiles(htmlWindow) {
    var dir = getOfxFileDirectory();
    if (!dir) {
        return;
    }
    for (var day in htmlWindow.ofxData) {
        var ofx = htmlWindow.ofxData[day];
        var fileName = expandFileName(dir.path);
        fileName.append('ib_' + day + '.ofx');
        writeStringToFile(fileName.path, ofx);
    }
}

function waitForOfxDownload(htmlWindow, maxTries, tryNumber) {
    tryNumber=tryNumber || 0;
    if (htmlWindow.ofxStatus == 'done') {
        mDebug.print("OFX download finished");
        writeOfxFiles(htmlWindow);
    } else if (htmlWindow.ofxStatus == 'error') {
        mDebug.print("There was an error in OFX download");
    }
    else if (tryNumber >= maxTries) {
        mDebug.print("Waited more the " + maxTries + " seconds for ofx download");
        return;
    } else {
        window.setTimeout(waitForOfxDownload, 1000, htmlWindow, maxTries, tryNumber+1);
    }

}

function doDownload() {
    var htmlWindow = content.document.getElementById('content').contentWindow.wrappedJSObject;
    if (htmlWindow) {
        setupLoadScript(htmlWindow);
        htmlWindow.loadScript('~/myconfig/firefox/ib-ofx-download-inject.js');
        htmlWindow.startOfxDownload(20);
        waitForOfxDownload(htmlWindow, 60);
    } else {
        alert("content window not found");
    }
}

function checkForDownloadActivity(maxTries, tryNumber) {
    tryNumber=tryNumber || 0;
    if (isDownloadActivityPage()) {
        doDownload();
    } else if (tryNumber < maxTries) {
        window.setTimeout(checkForDownloadActivity, 1000, maxTries, tryNumber+1);
    } else {
        alert('Timeout waiting for download activities page to load');
    }
}

function openDownloadActivities() {
    var htmlWindow = content.document.getElementById('navigation').contentWindow.wrappedJSObject;
    if (htmlWindow) {
        if (isDownloadActivityPage()) {
            doDownload();
        } else {
            htmlWindow.RM_DOWNLOAD_ACTIVITY.submit();
            checkForDownloadActivity(6);
        }
    } else {
        alert('Navigation frame not found');
    }
}

openDownloadActivities();



