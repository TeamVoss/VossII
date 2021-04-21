// Copyright 2021 Dorian Lesbre
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 		http://www.apache.org/licenses/LICENSE-2.0
//

// ==============================================
// Imports
// ==============================================

// The module 'vscode' contains the VS Code extensibility API
const vscode = require('vscode');
const fs = require('fs');


// ==============================================
// Globals
// ==============================================


const config = vscode.workspace.getConfiguration("vossii-fl")
const fl_in = `${config.temporary_files_root}in`;
const fl_prein = `${config.temporary_files_root}prein`;
const fl_out = `${config.temporary_files_root}out`;

const no_help = /^(;|\||\/\\|=>|\d+.*|.\d+.*|\/\/.*|andlettype|add_open_overload|assuming|begin_abstype|binder|binder_with_accumulator|clear_fixities|clet|cletrec|defix|end_abstype|export_to_tcl|forward_declare|free_binder|if_then_else_binder|then_binder|else_binder|infix|infix_unary|infixr|then|in|install_print_function|let|letrec|lettype|list|new_type_abbrev|non_lazy|nonfix|open_overload|overload|postfix|prefix|print_fixities|ref|val)$/;
const unescaped_quotes = /(?<!\\)"/;
const word_chars = "a-zA-Z0-9_";
const operator_chars = "\\-\\+\\*\\/\\\\\\%\\^\\'\\&\\|\\?\\~\\#\\<\\=\\>\\!\\%\\@\\:";

const sleep = (delay) => new Promise((resolve) => setTimeout(resolve, delay));

var terminal = null;
var known_functions = {};


// ==============================================
// Helper functions
// ==============================================


// send text to fl
function send_to_fl(code, file=fl_in) {
	// yes this is an extremly ugly way to write in a file
	// it's also the best I found...
	terminal.sendText(`echo ";" >> ${file}`);
	const lines = code.split("\n");
	for (const line of lines) {
		// replaced escaped quotes to keep them escaped in fl_in
		// split around non escaped quotes to safely add them
		const portions = line.replaceAll('\\"', '\\\\\\"').split(unescaped_quotes);
		const text = portions.join('"\'"\'"')
		terminal.sendText(`echo "${text}" >> ${file}\n`);
	}
}

// returns the full word (or operator) at given position
function word_at_position(string, position) {
	const word_regex = new RegExp("[" + word_chars + "]");
	const op_regex = new RegExp("[" + operator_chars + "]");
	const char = string.slice(position, position+1);
	var base;
	if (word_regex.test(char)) {
		base = word_chars;
	}
	else if (op_regex.test(char)) {
		base = operator_chars;
	}
	else return { left: -1, right: -1 };
	const start = new RegExp("[" + base + "]+$");
	const end = new RegExp("[^" + base + "]");
	const left = string.slice(0, position + 1).search(start);
	var right = string.slice(position).search(end);
	// Search for the word's beginning and end.
	if (right < 0) { right = string.length; }
	else { right += position; }
	return {left: left, right: right};
}

// returns the word or operator under the cursor
function text_under_cursor() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) return "";
	const position = editor.document.offsetAt(editor.selection.active);
	const text = editor.document.getText();
	const range = word_at_position(text, position);
	if (range.left == -1) return "";
	return text.slice(range.left, range.right);
}

// returns the selection text
function current_selection() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) return "";
	const text = editor.document.getText(editor.selection);
	if (text == "") return text_under_cursor();
	return text;
}

function str_reverse(string) {
	return string.split("").reverse().join("");
}

// returns current paragraph
function current_paragraph() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) return "";
	const position = editor.document.offsetAt(editor.selection.active);
	const text = editor.document.getText();
	const regex = /\n\s*\n/; // paragraph separator

	// find paragraph start
	var left = str_reverse(text.slice(0, position)).search(regex);
	if (left == -1)
		left = 0;
	else
		left = position - left;

	// find paragraph end
	var right = text.slice(position).search(regex);
	if (right == -1)
		right = text.length;
	else
		right = position + right;

	return text.slice(left, right);
}


// ==============================================
// Helper for hover and go to def
// ==============================================


// get help text from fl and memorizes it
async function get_help(funcname) {
	if (known_functions[funcname]) return known_functions[funcname];
	// get fl's help on identifier
	send_to_fl(`print(help "${funcname}");`);
	await sleep(100); // we need to wait a bit before reading...
	const content = fs.readFileSync(fl_out);
	//  extract the actual help message from all of fl's output
	const pos_fun = content.lastIndexOf(`Function: ${funcname}`);
	const pos_fail = content.lastIndexOf("Failure:");
	if (pos_fail >= pos_fun) return "";
	// save and return
	known_functions[funcname] = content.slice(pos_fun, -3);
	return known_functions[funcname];
}

// check if we are in a string or comment
function in_string_or_comment(text) {
	if (text.lastIndexOf("//") > text.lastIndexOf("\n"))
		return true;
	const regex = new RegExp(unescaped_quotes.source, "g");
	const count = (text.match(regex) || []).length;
	return count % 2 != 0;
}

async function help_from_pos(document, position) {
	const pos = document.offsetAt(position);
	const text = document.getText();
	if (in_string_or_comment(text.slice(0, pos)))
		return null;
	const indexes = word_at_position(text, pos);
	if (indexes.left == -1) return null;
	const ident = text.slice(indexes.left, indexes.right);
	const range = new vscode.Range(
		document.positionAt(indexes.left), document.positionAt(indexes.right)
	);
	if (no_help.test(ident))
		return {ident: ident, help: null, range: range, is_function: false};
	const help = String(await get_help(ident));
	return {ident: ident, help: help, range: range, is_function: true};
}


// ==============================================
// VS Code tooltips and go to definition
// ==============================================


async function get_tooltip(document, position) {
	// 1 - get identifier or operator under cursor
	const res = await help_from_pos(document, position);
	if (!res.is_function) {
		if (res.ident == "=>" || res.ident == "|")
			return {
				contents: ["if then else construct:\n\n\tcondition => if_true | if_false\n\tIF condition THEN if_true ELSE if_false"],
				range: res.range
			};
		return null;
	}
	if (!res.help)
		return { contents: [`Unknown function "${res.ident}"\n\nRun the file to define all functions`], range: res.range };
	const regex = /Function:\s*.*\s*(?:File:(?:.|\n)*?\nEnd:\s*\d+|Built-in)\s*(?:Implicit dependencies:\s*((?:.|\n)*?)\s*)?\s*(?:is the overloading of:\s*((?:.|\n)*?))?\s*(?:Arguments:\s*((?:.|\n)*?))?\s*(?:Return type:\s*(.*?))?\s*Fixity:\s*((?:.|\n)*?)\s*(?:Description:\s*((?:.|\n)*?))?\s*$/;
	const match = res.help.match(regex);
	if (match !== null) {
		// known format -> we can render a pretty tooltip
		var args_display = ""
		if (match[3]) // arguments
			args_display = match[3].replace(/\s*\n\s*/g, "} {").replaceAll("::", ":").replace(/\s*:\s*/g, "::").replace(/arg\. (\d+)/g, "arg$1");
		if (args_display)
			args_display = " {" + args_display + "}";
		var ret_display = "{return::void}";
		if (match[4]) // return type
			ret_display = `{return::${match[4]}}`;
		var fixity_display = match[5].replace("\nNon-lazy", " non-lazy").replace("nonfix", "").trim();
		if (match[1]) // dependencies
			fixity_display = `${fixity_display}\n\nDependencies: ${match[1]}`.trim();
		const display = `\tlet ${res.ident}${args_display} =\n\t\t${ret_display}`;
		var contents = [display, fixity_display];
		if (match[2]) {
			// overloading
			const format = match[2].replace(/\s*\n\s*/g, "}\n\t{");
			contents[0] = `\toverload\n\t{${format}}`;
		}
		if (match[6]) {
			// Description
			contents.push('```None\n' + match[6].replaceAll("\n ", "\n") + '\n```');
		}
		return {contents: contents, range: res.range};
	}
	// basic tooltip
	var help = res.help.replaceAll("\n\n", "\n").replaceAll("\n", "\n\n");
	help = help.replace("\n\nNon-lazy:", ",\tnon-lazy:")
	help = help.replace(/(File:(.|\n)*\nEnd:\s*\d+|Built-in)\s*/, "");
	return {contents: [help], range: res.range};
}

async function go_to_definition(document, position) {
	const res = await help_from_pos(document, position);
	if (!res.is_function || !res.help) return null;
	const match = res.help.match(/File:\s*(.*)\nStart:\s*(\d*)/);
	if (match === null) return null;
	const file = vscode.Uri.file(match[1].trim());
	const line = Number(match[2]);
	const vs_pos = new vscode.Position(line-1, 0);
	return new vscode.Location(file, vs_pos);
}


// ==============================================
// Extension commands
// ==============================================


function stop_fl() {
	send_to_fl("quit;");
	terminal.sendText(`rm -f "${fl_in}" "${fl_out}" "${fl_prein}"`);
}

function start_fl() {
	stop_fl();
	if (config.fl_setup) send_to_fl(config.fl_setup, fl_prein);
	else terminal.sendText(`echo "" > ${fl_prein}`);
	known_functions = {};
	terminal.sendText(`${config.path} --read_input_from_file "${fl_in}" -f "${fl_prein}" -use_stdout >& "${fl_out}" &`);
}

function help() {
	const text = text_under_cursor()
	if (text) send_to_fl(`print(help "${text}");`);
}

function eval_selection() {
	const text = current_selection();
	if (text) send_to_fl(`${text};`);
}

function eval_paragraph() {
	const text = current_paragraph();
	if (text) send_to_fl(`${text};`);
}

async function eval_file() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) return;
	const document = editor.document;
	if (document.isDirty) {
		// prompt to save documents
		if (config.save_file_on_run == "Ask me") {
			const response_save = "Save";
			const response_unsave = "Don't save"
			const response = await vscode.window.showWarningMessage(
				"File unsaved, ok to save ?", response_save, response_unsave
			);
			if (response == response_save)
				await document.save();
		}
		else if (config.save_file_on_run == "Autosave")
			await document.save();
	}
	const filename = document.fileName;
	send_to_fl(`load "${filename}";`);
}

function eval_line() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) return;
	const line = editor.selection.active.line;
	const range = new vscode.Range(new vscode.Position(line, 0), new vscode.Position(line+1, 0));
	const text = editor.document.getText(range);
	send_to_fl(text);
}

async function restart_and_eval_file() {
	start_fl();
	await sleep(500); // wait for fl to start
	eval_file();
}


// ==============================================
// VS Code extension configuration
// ==============================================


/**
 * this method is called when the extension is activated
 *
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {

	terminal = vscode.window.createTerminal("fl terminal");

	// commands
	const functions = [
		{name: 'fl.start', func: start_fl},
		{name: 'fl.stop', func: stop_fl},
		{name: 'fl.help', func: help},
		{name: 'fl.eval_selection', func: eval_selection},
		{name: 'fl.eval_paragraph', func: eval_paragraph},
		{name: 'fl.eval_file', func: eval_file},
		{name: 'fl.eval_line', func: eval_line},
		{name: 'fl.restart_and_eval_file', func: restart_and_eval_file},
	]
	for (let index = 0; index < functions.length; index++) {
		const obj = functions[index];
		const disposable = vscode.commands.registerCommand(obj.name, obj.func);
		context.subscriptions.push(disposable);
	}

	// hover provider
	const disposable = vscode.languages.registerHoverProvider('fl', {
		provideHover(document, position, _token) {
			return get_tooltip(document, position);
		}});
	context.subscriptions.push(disposable);

	// go to definition
	const disposable2 = vscode.languages.registerDefinitionProvider('fl', {
		provideDefinition(document, position, _token) {
			return go_to_definition(document, position);
		}})
	context.subscriptions.push(disposable2);

			// vscode.languages.registerCompletionItemProvider("fl", {
			// 	provideCompletionItems(document, position, _token, context) {
			// 		terminal.sendText(`echo "called ${context.triggerKind}"`);
			// 		vscode.window.showInformationMessage(context.triggerCharacter);
			// 	}
			// })

	if (config.run_on_startup)
		start_fl();
}

// this method is called when the extension is deactivated
function deactivate() {
	stop_fl();
	terminal.hide();
	terminal.dispose();
}

// eslint-disable-next-line no-undef
module.exports = {
	activate,
	deactivate
}
