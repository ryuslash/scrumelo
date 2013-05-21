(function ($) {
    $(document).ready(function () {
        $(".hide").hide();
        $(".toggle").click(function () {
            $(document.getElementById($(this).data("show"))).toggle();
        });
    });
})(jQuery);
