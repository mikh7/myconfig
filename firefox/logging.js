var MM_DEBUG_MESSAGE = 1;
var MM_LOG_MESSAGE = 2;


function mmLogToConsole(aMessage, aSourceName, aSourceLine, aLineNumber, 
                        aColumnNumber, aFlags, aCategory)
{
  var consoleService = Components.classes["@mozilla.org/consoleservice;1"]
                                 .getService(Components.interfaces.nsIConsoleService);
  var scriptError = Components.classes["@mozilla.org/scripterror;1"]
                              .createInstance(Components.interfaces.nsIScriptError);
  scriptError.init(aMessage, aSourceName, aSourceLine, aLineNumber, 
                   aColumnNumber, aFlags, aCategory);
  consoleService.logMessage(scriptError);
}

var mDebug = {
   _initialized: false,
   _dbgService: true,
   _init: function() {
      try {
         this._consoleService = Components.classes[ "@mozilla.org/consoleservice;1" ];
         this._consoleService = this._consoleService.getService( Components.interfaces.nsIConsoleService );
         this._initialized = true;
      }
      catch( e ) {
      }
   },

   Timer: function( name ) {
       var t1, t2, delta, total = 0, count = 0;
       this.start  = function() { ++count; t1 = new Date(); return this; };
       this.end    = function() { t2 = new Date(); delta = t2 - t1; total += delta; return this; };
       this.print  = function() { 
           if (count > 1) {
               /*
               repl.print("Timer (" + name + "): " + delta + " ms" +
                            "; called " + count + " times" +
                            "; total: " + total + " ms" +
                            "; avg time per call: " + (total / count) + " ms"
                            ); 
               */
               mDebug.print("Timer (" + name + "): " + delta + " ms" +
                            "; called " + count + " times" +
                            "; total: " + total + " ms" +
                            "; avg time per call: " + (total / count) + " ms"
                            ); 
           } else {
               /*
               repl.print("Timer (" + name + "): " + delta + " ms");
               */
               mDebug.print("Timer (" + name + "): " + delta + " ms");
           }
       };
   },

   timedFunc: function( func, desc ) {
        if (typeof arguments.callee.timer == "undefined") {
            arguments.callee.timer = {};
        }
        var id = desc || func.desc || func.name;
        if (typeof arguments.callee.timer[id] == "undefined") {
            arguments.callee.timer[id] = new mDebug.Timer(id);
        }
        var timer = arguments.callee.timer[id];

        return function() {
            timer.start();
            var ret = func.apply(null, arguments);
            timer.end();
            timer.print();
            return ret;
        };
   },

   on: function( refresh ) {
      if( !this._initialized ) {
         this._init()
      }
      if( this._initialized ) {
         if( refresh == null ) {
            refresh = false;
         }
         return this._dbgService;
      }
      return false;
   },
   
   print: function( message, type ) {
      if( !this._initialized ) {
         this._init()
      }
      if( this._initialized ) {
          var stackFrame = Components.stack.caller;
          var filename = stackFrame.filename;
          filename = filename.replace(/.* -> /g, "");
          var lineNumber = stackFrame.lineNumber;
          var columnNumber = stackFrame.columnNumber;
/*          if (filename.indexOf("ybSidebarOverlay.xml") < 0 && filename.indexOf("Debug") < 0) return;*/
         if( type == null ) {
            type = MM_DEBUG_MESSAGE;
         }
          if( this.on()) {
              // if (filename == "ybSidebarOverlay.xml") {
              mmLogToConsole(message, filename, null, lineNumber, columnNumber, Components.interfaces.nsIScriptError.warningFlag, 1);
              // }
          }
      }
   },

   printStack: function(message) {
        var consoleService = Components.classes["@mozilla.org/consoleservice;1"]
                                 .getService(Components.interfaces.nsIConsoleService);

        var stackFrame = Components.stack.caller;
        var arr = [message];
        var msg;
        while (stackFrame) {
            var filename = stackFrame.filename;
            if (filename == null) break;
            var lineNumber = stackFrame.lineNumber;
            arr.push("    " + filename + ":" + lineNumber);
            stackFrame = stackFrame.caller;
        }

        msg = arr.join("\n");
        stackFrame = Components.stack.caller;
        lineNumber = stackFrame.lineNumber;
        columnNumber = stackFrame.columnNumber;
        filename = stackFrame.filename;
        mmLogToConsole(msg, filename, null, lineNumber, columnNumber, Components.interfaces.nsIScriptError.errorFlag, 1);
   },

   assert: function( boolValue, msg ) {
        if (boolValue) return true; 
        else {
            msg = "Assertion failed: " + msg;
            var stackFrame = Components.stack.caller.caller;
            var filename = stackFrame.filename;
            var lineNumber = stackFrame.lineNumber;
            var columnNumber = stackFrame.columnNumber;
            
            // mDebug.print(msg);
            mmLogToConsole(msg, filename, null, lineNumber, columnNumber, Components.interfaces.nsIScriptError.errorFlag, 1);
            return false;
        }
   },
   
   /* below to be used only for debugging*/
   printOutArcs: function( datasource, resource ) {

   	  mDebug.print( " *********************" );
   	  var properties = datasource.ArcLabelsOut( resource );


   	  while ( properties.hasMoreElements() ) {
   	    var s = properties.getNext();
   	    s.QueryInterface( Components.interfaces.nsIRDFResource );
   	    var target = datasource.GetTarget ( resource, s, true );
   	    try {
   	      target.QueryInterface ( Components.interfaces.nsIRDFLiteral );
   	      mDebug.print ( "Literal:" + s.Value + " => " + target.Value);
   	    } catch (e) {
   	      try {
   	        target.QueryInterface ( Components.interfaces.nsIRDFResource );
   	        mDebug.print ( resource.Value + ": Resource => " + s.Value + " => " + target.Value );
   	      } catch (e) {

   	        try {
   	          target.QueryInterface ( Components.interfaces.nsIRDFDate );
   	          mDebug.print ( "Date: " + s.Value + " => " + target.Value );
   	        } catch (e) {
   	        }

   	      }
   	    }

   	  }
   },
   
    printObject: function(aObject, message) {
        var str = "";
        if (!message) message = "";
        str += message + ": ";     
        if (aObject == null) {
            str += "(null)";
            mDebug.print(str);
            return;
        }
        str += "{\n";
        for (var prop in aObject) {
            if (aObject.hasOwnProperty(prop)) {
                str += "    [" + prop + "]: " + "\"" + aObject[prop] + "\"\n";
            }
        }
        str += "}\n";
        mDebug.print(str);
    }
   
};

mDebug.print("mDebug loaded");
try {
    dactyl.mDebug = mDebug
} catch (e) {}
