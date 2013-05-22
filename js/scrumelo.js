(function ($) {
    $(document).ready(function () {
        $(".hide").hide();
        $(".toggle").click(function () {
            $(document.getElementById($(this).data("show"))).toggle();
        });
    });
})(jQuery);

function get_story_info(element) {
    var id = element.id;
    var data_element = $(element).parent().find(".data");

    if (data_element.length > 0)
        data_element.remove();
    else
        $.get('/stories/' + id, null,
              function (data, textStatus, jqXHR) {
                  $(element).after("<div class=\"data\">" +
                                   "Assignee: " + data.Assignee +
                                   "<pre>" + data.content + "</pre>" +
                                   "</div>");
              }, 'json');
}
