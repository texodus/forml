
(function() {
jasmine.FormalReporter = function(doc) {
   this.document = doc || document;
   this.suiteDivs = {};
   this.logRunningSpecs = false;
 };
 
 jasmine.FormalReporter.prototype.createDom = function(type, attrs, childrenVarArgs) {
 };
 
 jasmine.FormalReporter.prototype.reportRunnerStarting = function(runner) {
   var showPassed, showSkipped;
 
   var suites = runner.suites();
   for (var i = 0; i < suites.length; i++) {
     var suite = suites[i];
   }
 
   this.startedAt = new Date();
 
   var self = this;
 };
 
 jasmine.FormalReporter.prototype.reportRunnerResults = function(runner) {
 };
 
 jasmine.FormalReporter.prototype.reportSuiteResults = function(suite) {
 };
 
 jasmine.FormalReporter.prototype.reportSpecStarting = function(spec) {
 };
 
 jasmine.FormalReporter.prototype.reportSpecResults = function(spec) {
   var results = spec.results();
   var status = results.passed() ? 'passed' : 'failed';
 
     var k = "#test_" + spec.description.split("::")[0]
     var el = $(k)

     if (status == "passed") {
         if (k.indexOf("multi") >= 0) {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:#20BA18'> <span style='color:rgb(255,255,255);font-weight:bold'>PASS</span> </span> ")
         } else {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:#20BA18'> <span style='color:rgb(255,255,255);font-weight:bold'>PASS</span> </span> ")
         }
     } else {
         if (k.indexOf("multi") >= 0) {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:#BA1820'> <span style='color:rgb(255,255,255);font-weight:bold'>FAIL</span> </span> ")
         } else {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:#BA1820'> <span style='color:rgb(255,255,255);font-weight:bold'>FAIL</span> </span> ")
         }
     }
 };
 
 jasmine.FormalReporter.prototype.log = function() {
 };
 
 jasmine.FormalReporter.prototype.getLocation = function() {
   return this.document.location;
 };
 
 jasmine.FormalReporter.prototype.specFilter = function(spec) {
   var paramMap = {};
   var params = this.getLocation().search.substring(1).split('&');
   for (var i = 0; i < params.length; i++) {
     var p = params[i].split('=');
     paramMap[decodeURIComponent(p[0])] = decodeURIComponent(p[1]);
   }
 
   if (!paramMap.spec) {
     return true;
   }
   return spec.getFullName().indexOf(paramMap.spec) === 0;
 };
 
})();