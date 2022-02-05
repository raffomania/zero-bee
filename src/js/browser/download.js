export default function downloadBackup(data) {
    data = JSON.stringify(data);
    var elem = window.document.createElement("a");
    var blob = new Blob([data], { type: "application/json" });
    elem.href = window.URL.createObjectURL(blob);
    var year = new Date().getFullYear();
    var month = parseInt(new Date().getMonth(), 10) + 1;
    elem.download = `zero_bee_backup_${year}_${month}.json`;
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
}
