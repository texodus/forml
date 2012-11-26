
(function() {
jasmine.FormlReporter = function(doc) {
   this.document = doc || document;
   this.suiteDivs = {};
   this.logRunningSpecs = false;
 };
 
 jasmine.FormlReporter.prototype.createDom = function(type, attrs, childrenVarArgs) {
 };
 
 jasmine.FormlReporter.prototype.reportRunnerStarting = function(runner) {
   var showPassed, showSkipped;
 
   var suites = runner.suites();
   for (var i = 0; i < suites.length; i++) {
     var suite = suites[i];
   }
 
   this.startedAt = new Date();
 
   var self = this;
 };
 
 jasmine.FormlReporter.prototype.reportRunnerResults = function(runner) {
     $("#run_tests").unbind();
     $("header #test_button").css({"background-color":"lightgreen"});
     $("header #test_button a, header #test_button strong").css({"color":"white"});
 };
 
 jasmine.FormlReporter.prototype.reportSuiteResults = function(suite) {
 };
 
 jasmine.FormlReporter.prototype.reportSpecStarting = function(spec) {
 };
 
 jasmine.FormlReporter.prototype.reportSpecResults = function(spec) {
   var results = spec.results();
   var status = results.passed() ? 'passed' : 'failed';
 
     var k = "#test_" + spec.description.split("__::__")[0]
     var el = $(k)

     spec.description = spec.description.split("__::__")[1]

     if (status == "passed") {
         if (k.indexOf("multi") >= 0) {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:rgba(32,186,24,0.5)' title='" + JSON.stringify(spec.results_.items_[0].actual) + "'> <span style='color:rgb(255,255,255);font-weight:bold'>PASS</span> </span> ")
         } else {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:rgba(32,186,24,0.5)' title='" + JSON.stringify(spec.results_.items_[0].actual) + "'> <span style='color:rgb(255,255,255);font-weight:bold'>PASS</span> </span> ")
         }
     } else {
         if (k.indexOf("multi") >= 0) {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:rgba(182,24,32,0.5)' title='" + JSON.stringify(spec.results_.items_[0].actual) + "'> <span style='color:rgb(255,255,255);font-weight:bold'>FAIL</span> </span> ")
         } else {
             $(k).before("<span style='border-radius:3;margin-right:-50px;background-color:rgba(182,24,32,0.5)' title='" + JSON.stringify(spec.results_.items_[0].actual) + "'> <span style='color:rgb(255,255,255);font-weight:bold'>FAIL</span> </span> ")
         }
     }
 };
 
 jasmine.FormlReporter.prototype.log = function() {
 };
 
 jasmine.FormlReporter.prototype.getLocation = function() {
   return this.document.location;
 };
 
 jasmine.FormlReporter.prototype.specFilter = function(spec) {
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