
exports.toast = function(str) {
    return function () {
      Toastify({
        text: str,
        duration: 5000,
        gravity: "top",
        position: "center",
        backgroundColor: "#00b09b"
      }).showToast();
    }
}


