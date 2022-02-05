import { save } from "@tauri-apps/api/dialog";
import { writeFile } from "@tauri-apps/api/fs";

export default function download(data) {
    var year = new Date().getFullYear();
    var month = parseInt(new Date().getMonth(), 10) + 1;

    save({
        defaultPath: `zero_bee_backup_${year}_${month}.json`,
        filters: [{ name: "JSON Files", extensions: ["json"] }],
    }).then((path) => {
        writeFile({
            contents: JSON.stringify(data),
            path,
        }).catch((e) => {
            console.error(e);
            alert(`Error while saving backup file: ${e}`);
        });
    });
}
