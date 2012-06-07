
(function() {
    if (! jasmine) {
        throw new Exception("jasmine library does not exist in global namespace!");
    }

    var console = { log: global.console.log }
    var messages = []

    global.console.log = function() {}

    /**
     * Basic reporter that outputs spec results to the browser console.
     * Useful if you need to test an html page and don't want the TrivialReporter
     * markup mucking things up.
     *
     * Usage:
     *
     * jasmine.getEnv().addReporter(new jasmine.ConsoleReporter());
     * jasmine.getEnv().execute();
     */
    var ConsoleReporter = function() {
        this.started = false;
        this.finished = false;
    };

    ConsoleReporter.prototype = {
        reportRunnerResults: function(runner) {
            if (this.hasGroupedConsole()) {
                var suites = runner.suites();
                startGroup(runner.results(), 'tests');
                for (var i in suites) {
                    if (!suites[i].parentSuite) {
                        suiteResults(suites[i]);
                    }
                }
                console.groupEnd();
            }
            else {
                var dur = (new Date()).getTime() - this.start_time;
                var failed = this.executed_specs - this.passed_specs;
                var spec_str = this.executed_specs + (this.executed_specs === 1 ? " spec, " : " specs, ");
                var fail_str = failed + (failed === 1 ? " failure in " : " failures in ");
                var red = (failed > 0) ? "\033[31m" : "\033[32m"
                this.log(red);
                this.log(spec_str + fail_str + (dur/1000) + "s.\033[0m");
                if (failed > 0) {
                    process.stdout.write("\r[\033[31mX\033[0m] Running Tests\n\n")
                    for (msg in messages) { process.stdout.write(messages[msg]) }
                } else {
                    process.stdout.write("\r[\033[32m*\033[0m] Running Tests\n")
                }

            }
            this.finished = true;
        },

        hasGroupedConsole: function() {
            var console = jasmine.getGlobal().console;
            return console && console.info && console.warn && console.group && console.groupEnd && console.groupCollapsed;
        },

        reportRunnerStarting: function(runner) {
            this.started = true;
            if (!this.hasGroupedConsole()) {
                this.start_time = (new Date()).getTime();
                this.executed_specs = 0;
                this.passed_specs = 0;
                process.stdout.write("[ ] Running Tests");
            }
        },

        reportSpecResults: function(spec) {
            if (!this.hasGroupedConsole()) {
                var resultText = "\033[31mFailure\033[0m";
                var x = spec.results()
                if (x.passed()) {
                    this.passed_specs++;
                } else {
                    messages.push("Test failed: " + spec.description.split("::")[1] + "\n" 
                                  + "    at line " 
                                  + (parseInt(spec.description.split("::")[0].split("_")[0]) + 1)
                                  + "\n\n");
                    this.log(resultText + " in " + spec.suite.description + "\n" + spec.description);
                    this.log(x.items_[0].message)
                }
            }
        },

        reportSpecStarting: function(spec) {
            if (!this.hasGroupedConsole()) {
                this.executed_specs++;
                //this.log(spec.suite.description + ' : ' + spec.description + ' ... ');
            }
        },

        reportSuiteResults: function(suite) {
            if (!this.hasGroupedConsole()) {
                var results = suite.results();
                var red = (results.passedCount < results.totalCount) ? "\033[31m" : "\033[32m"
               
                this.log(red + suite.description + ": " + results.passedCount + " of " + results.totalCount + " passed.\033[0m");
            }
        },

        log: function(str) {
            var console = jasmine.getGlobal().console;
            if (console && console.log) {
                console.log(str);
            }
        }
    };

    function suiteResults(suite) {
        var results = suite.results();
        startGroup(results, suite.description);
        var specs = suite.specs();
        for (var i in specs) {
            if (specs.hasOwnProperty(i)) {
                specResults(specs[i]);
            }
        }
        var suites = suite.suites();
        for (var j in suites) {
            if (suites.hasOwnProperty(j)) {
                suiteResults(suites[j]);
            }
        }
        console.groupEnd();
    }

    function specResults(spec) {
        var results = spec.results();
        startGroup(results, spec.description);
        var items = results.getItems();
        for (var k in items) {
            if (items.hasOwnProperty(k)) {
                itemResults(items[k]);
            }
        }
        console.groupEnd();
    }

    function itemResults(item) {
        if (item.passed && !item.passed()) {
            console.warn({actual:item.actual,expected: item.expected});
            item.trace.message = item.matcherName;
            console.error(item.trace);
        } else {
            console.info('Passed');
        }
    }

    function startGroup(results, description) {
        var consoleFunc = (results.passed() && console.groupCollapsed) ? 'groupCollapsed' : 'group';
        red = (results.failedCount > 0) ? "\033[31m" : ""
        console[consoleFunc](red + description + ' (' + results.passedCount + '/' + results.totalCount + ' passed, ' + results.failedCount + ' failures)\033[0m');

    }

    // export public
    jasmine.ConsoleReporter = ConsoleReporter;
})();

 jasmine.getEnv().addReporter(new jasmine.ConsoleReporter());
 jasmine.getEnv().execute();