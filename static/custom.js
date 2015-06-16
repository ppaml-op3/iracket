require(['base/js/events'], function (events) {
    'use strict';
    events.on('app_initialized.NotebookApp', function () {
        IPython.load_extensions('ic3');
    });
});
