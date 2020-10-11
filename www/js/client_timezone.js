$(document).on('shiny:connected', function(event) {
    var timezone = Intl.DateTimeFormat().resolvedOptions().timeZone
    Shiny.setInputValue('client_timezone', timezone);
});
