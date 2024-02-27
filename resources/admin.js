import { initButtonList } from "./admin/buttonList.js"
import { initCalendar } from './admin/calendar'
import { initCopyClipboard } from "./admin/copyClipboard.js";
import { initDuplicateSessionForm } from "./admin/duplicateSessions.js";
import { initFilterForm } from "./admin/filter.js"
import { initPrint } from "./admin/print"
import { initRichTextEditor } from "./admin/richTextEditor.js"
import { initHtmxSearch } from "./admin/search.js"


initButtonList()
initCalendar();
initCopyClipboard();
initDuplicateSessionForm();
initFilterForm();
initPrint();
initRichTextEditor();
initHtmxSearch();

window['pool-tool'] = {
    initRichTextEditor
};

