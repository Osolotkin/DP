import { createHighlighter } from 'shiki';
import { readFileSync, writeFileSync } from 'node:fs';
import { JSDOM } from 'jsdom';

const grammar = JSON.parse(readFileSync('../tools/VS-Code/language-support/syntaxes/vi.tmLanguage.json', 'utf8'));
const hg = await createHighlighter({
    langs: [grammar],
    themes: ['nord', 'dracula-soft']
});



const document = new JSDOM(readFileSync('doc_tmp.html')).window.document;
document.querySelectorAll('pre').forEach((block) => {
    block.innerHTML = hg.codeToHtml(block.textContent, { lang: 'vi', theme: 'dracula-soft' });
});

writeFileSync('doc.html', document.documentElement.outerHTML);