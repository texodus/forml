

$(function() {
    var headers = $("h2,h3,h4,h5,h6")

    var text = ""

    headers.map(function(y,x) {
	text += "<a href=\"#" + x.innerHTML.toLowerCase().replace(/\s/g, "-") + "\">"  + x.innerHTML + "</a></br>"
    });

    $('#contents').html(text);
    $('body').css('font-family','Arial');
    $('code').css('font-family', 'Menlo, Consolas, Monospace');
    $('p, ul, ol').css('text-align','justify')
    $('code').addClass('prettyprint lang-hs');
    $('body').css('margin', '0 0');
    $('pre').css('margin', '0 0 23px 0');

    prettyPrint();

    jasmine.getEnv().addReporter(new jasmine.FormalReporter());
    jasmine.getEnv().addReporter(new jasmine.TrivialReporter());
    jasmine.getEnv().execute();

    var reporter = $(".jasmine_reporter").detach();

    $("#main").append(reporter)
});

function time(f) {
    var start = new Date().getTime();
    
    f();

    var end = new Date().getTime();
    return end - start;
};

function fibb(n) {
    if (n === 0) {
    	return 0;
    } else if (n === 1) {
    	return 1;
    } else {
    	return fibb(n - 1) + fibb(n - 2)
    }
}
