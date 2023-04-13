"use strict";

export function loadFile(url, onProgress, onReady, onError) {
    const xhr = new XMLHttpRequest();
    xhr.responseType = 'text';

    xhr.onload = () => {
        if (xhr.readyState === xhr.DONE) {
            if (xhr.status === 200) {
                onReady(xhr.responseText)
            }
        }
    };

    xhr.addEventListener("progress", function (e) {
        if (e.lengthComputable) {
            onProgress(e.loaded, e.total)
        }
    });

    xhr.addEventListener("error", function (e) {
        onError()
    });

    xhr.open("GET", url);
    xhr.send();
}