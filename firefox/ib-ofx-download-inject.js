/**
 * Interactive brokers account management last N days QFX downloader
 */

if (!('Prototype' in this)) {
    loadScript('~/prototype.js');
}

if (!('addDays' in Date.prototype)) {
    loadScript('~/date.js');
}


function downloadOneDay(day) {
    var today = Date.today();
    if (!day.isBefore(today)) {
        //console.log("downloadOneDay() returning because " + day
        //            + " is not before " + today);
        console.log("Today reached, done");
        window.ofxStatus = 'done';
        return;
    }
    var dat = $('dat');
    dat.format.value = '5'; // quicken
    console.log('Downloading day ' + day);
    var dayStr = day.toString('yyyyMMdd');
    dat.dates.value = dayStr;
    var req = new Ajax.Request(
        dat.action,
        {method: dat.method,
         parameters: dat.serialize(true),
         // onSuccess: function () {
         //     console.log("Success!");
         //     // var response = transport.responseText || "no response text";
         //     console.log("Success!");
         // }
         // ,
         onComplete: function (transport) {
             var response = transport.responseText || "no response text";
             //console.log('here response=' + response);
             if (response.search('function fetchReport') == -1) {
                 console.log("Unable to find function fetchReport");
                 window.ofxStatus = 'error';
                 return;
             }
             var url = '../servlet/ReportManagement.Process.ProcessReport';
             new Ajax.Request
             (url,
              {method: 'get',
               onComplete: function(transport) {
                   if (transport.responseText='d') {
                       //console.log("Report is ready");
                       var url = 'ReportManagement.Process.DisplayProcessedReport';
                       new Ajax.Request
                       (url,
                        {method: 'get',
                         onComplete: function(transport) {
                             if (transport.status != 200) {
                                 console.log('Final download status is ' + transport.status);
                                 window.ofxStatus = 'error';
                                 return;
                             }
                             var resp = transport.responseText;
                             //console.log("Gotten report = " + resp);
                             window.ofxData[dayStr] = resp;
                             day.addDays(1);
                             downloadOneDay(day);
                         }});
                   }
                   else {
                       console.log("Some problem generating report");
                       window.ofxStatus = 'error';
                       return;
                   }
               }
              });}});
    //console.log('req=' + req);
}

/**
 * Should be called in the 'content' window after
 * it loaded the "Download activity" page
 * logging it into account management page.
 */
function startOfxDownload(numDays) {
    numDays=numDays || 10
    var dat = $('dat');
    window.ofxStatus = 'started';
    window.ofxData = {};
    if (!dat) {
        window.ofxStatus = 'error';
        console.log("Unable to find form 'dat', are you on download page?");
        return;
    }
    var day = Date.today().addDays(-numDays)
    downloadOneDay(day);
}

console.log('Loaded');

