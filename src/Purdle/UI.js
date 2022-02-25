
exports.toast = function(dur) {
    return function(str) {
        return function () {
          Toastify({
            text: str,
            duration: dur,
            gravity: "top",
            position: "center",
            // backgroundColor: "#00b09b"
          }).showToast();
        }
    }
}


