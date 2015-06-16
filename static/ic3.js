define([
    'notebook/js/outputarea',
    'c3'
], function (oa, c3) {
    "use strict";

    // IPython doesn't know how to automatically load CSS.
    var load_css = function () {
        var link = document.createElement("link");
        link.type = "text/css";
        link.rel = "stylesheet";
        link.href = require.toUrl("./c3.css");
        document.getElementsByTagName("head")[0].appendChild(link);
    };

    var load_ipython_extension = function () {
        load_css();
    };

    // Teach IPython how to display data for C3.
    oa.OutputArea.safe_outputs["application/x-c3-data"] = true;
    oa.OutputArea.display_order.unshift("application/x-c3-data");
    oa.OutputArea.append_map["application/x-c3-data"] = function (data, md, element) {
        var type = 'text/html';
        var toinsert = this.create_output_subarea(md, "output_html rendered_html", type);
        this.keyboard_manager.register_events(toinsert);

        var c3data = JSON.parse(data);
        delete c3data.bindto;

        var chart = c3.generate(c3data);
        toinsert.append(chart.element);
        element.append(toinsert);

        return toinsert;
    };

    return {load_ipython_extension:load_ipython_extension};
});
