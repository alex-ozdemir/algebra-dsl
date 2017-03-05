"use strict";

MathJax.Hub.Config({
    showMathMenu: false,
    ShowMathMenuMSIE: false,
    //showProcessingMessages: false,
    messageStyle: "none",
    CommonHTML: {
        scale: 200,
        linebreaks: {
            automatic: true
        }
    },
    MathML: {
        extensions: ["content-mathml.js"]
    },
    "content-mathml": {
        //collapsePlusMinus: true, // TODO(aozdemir) Do we want to use this?
        cistyles: {
            texttt: '',
            vector: 'bold-italic',
            matrix: 'bold-upright'
        },
    }
});

var currentCM = null;
var currentMath = null;
var CMhistory = []; //stores references to all the old CM instances
var isMousePressed = false;
var mousePressAnchor = null;
var batchCommands = [];
var modified = false;
var socket = new WebSocket("ws://" + location.hostname + ':2794', "rust-websocket");

function putIntoCM(cm, text, saveOld) {
    if (saveOld) {
        var oldVal = cm.getValue();
        if (oldVal.length > 0) {
            batchCommands.push(oldVal);
        }
    }
    cm.setValue(text);
    cm.setCursor(cm.posFromIndex(text.length));
}

$(document).ready(function() {
    $('#feedbackModal').on('shown.bs.modal', function () {
        $('#feedbackText').focus();
    })

    var cm = createCM();
    putIntoCM(cm, "$ ", false);
    currentCM = cm;
    modified = false;

    document.addEventListener("mouseup", globalMouseUpCallback);
})

function createCM() {
    if (currentMath) {
        removeMathCallbacks(currentMath);
    }

    var cm = CodeMirror(document.getElementById('repl'), {
        autofocus: true,
        matchBrackets: true,
        extraKeys: {
            'Enter': sendToServer,
            'Up': reclaimUp,
            'Down': reclaimDown,
        },
    });

    CMhistory.push(cm);
    cm.loc = 0; //helps us keep track of where we are in history
    cm.history = CMhistory.map(x => x.getValue()); //needed for bash-like continuity

    // Scroll down
    document.getElementById('repl').lastElementChild.scrollIntoView({
        behavior: "smooth",
        block: "end",
    });

    cm.selectedDOM = null;
    cm.selectedTextMarker = null;
    cm.prevMath = null;

    cm.on('change', replaceAllMathTags);
    cm.on('keydown',  solidifySelection);
    cm.on('mousedown',  solidifySelection);

    return cm;
}

function sendToServer(cmBox) {
    cmBox.setOption('readOnly', true);
    cmBox.setOption('cursorBlinkRate', -1);
    socket.send("cmd@" + cmBox.getValue());
}

function mouseDownCallback(event) {
    isMousePressed = true;
    mousePressAnchor = mathTreeNodeAbove(event.target);
}

function mouseUpCallback(event) {
    if (mousePressAnchor === null) return;

    var cm = currentCM;

    var mousePressHead = mathTreeNodeAbove(event.target);
    if (mousePressHead === mousePressAnchor) {
        if (cm.selectedDOM !== null) {
            var cur = event.target;
            while (cur !== this && cur !== cm.selectedDOM) {
                cur = cur.parentNode;
            }

            if (cur === cm.selectedDOM) {
                // Click is on same place -- expand

                var oldMarkerLoc = cm.selectedTextMarker.find();

                cm.selectedDOM.removeAttribute('highlighted');
                cm.selectedDOM.removeAttribute('selected');

                var oneHigher = mathTreeNodeAbove(cur.parentNode, this);
                if (oneHigher === null) {
                    // We hit the limit: just get rid of it
                    cm.replaceRange('', oldMarkerLoc.from, oldMarkerLoc.to, 'click-remove');
                    cm.selectedDOM = null;
                    cm.selectedTextMarker = null;
                } else {
                    var insertText = '#(mtn:'+oneHigher.getAttribute('mathtreenode')+')';
                    cm.replaceRange(insertText, oldMarkerLoc.from, oldMarkerLoc.to, 'click');
                }
            } else {
                // Click is on different place, so solidify old location
                solidifySelection();

                var cursorPos = cm.getCursor();
                var mtn = mathTreeNodeAbove(event.target, this);
                var insertText = '#(mtn:'+mtn.getAttribute('mathtreenode')+')';
                cm.replaceRange(insertText, cursorPos, cursorPos, 'click');
            }
        } else {
            var cursorPos = cm.getCursor();
            var mtn = mathTreeNodeAbove(event.target, this);
            var insertText = '#(mtn:'+mtn.getAttribute('mathtreenode')+')';
            cm.replaceRange(insertText, cursorPos, cursorPos, 'click');
        }
    } else {
        solidifySelection();

        var headAncestors = [];

        var cur = mousePressHead
        while (cur !== null) {
            headAncestors.push(cur);
            cur = mathTreeNodeOrMultiparentAbove(cur.parentNode, this);
        }
        cur = mousePressAnchor;
        var prev = null;
        var indexOfMatch
        while ((indexOfMatch = headAncestors.indexOf(cur)) === -1) {
            prev = cur;
            cur = mathTreeNodeOrMultiparentAbove(cur.parentNode, this);
        }

        if (!cur.hasAttribute('multiparent')) {
            // Not a multiparent: not allowed to select siblings, so just select
            // the whole thing
            var cursorPos = cm.getCursor();
            var insertText = '#(mtn:'+cur.getAttribute('mathtreenode')+')';
            cm.replaceRange(insertText, cursorPos);
        } else if (prev !== null && indexOfMatch != 0) {

            var siblingOne = headAncestors[indexOfMatch-1];
            var siblingTwo = prev;

            cur = siblingTwo;

            var siblingsToAdd = [cur];
            while (cur !== null && cur !== siblingOne) {
                cur = cur.nextSibling;
                if (cur !== null && cur.hasAttribute('mathtreenode')) {
                    siblingsToAdd.push(cur);
                }
            }
            if (cur === null) {
                cur = siblingOne;
                siblingsToAdd = [cur];
                while (cur !== siblingTwo) {
                    cur = cur.nextSibling;
                    if (cur.hasAttribute('mathtreenode')) {
                        siblingsToAdd.push(cur);
                    }
                }
            }

            var insertText = '';
            for (var i=0; i<siblingsToAdd.length; i++) {
                insertText += '#(mtn:'+siblingsToAdd[i].getAttribute('mathtreenode')+')';
            }

            var cursorPos = cm.getCursor();
            cm.replaceRange(insertText, cursorPos);
        }
    }
    mousePressAnchor = null;
    cm.focus();
}

function globalMouseUpCallback(event) {
    isMousePressed = false;
    mousePressAnchor = null;
}

function addMathCallbacks(where) {
    where.addEventListener("mousedown", mouseDownCallback);
    where.addEventListener("mouseup", mouseUpCallback);
}

function removeMathCallbacks(where) {
    where.removeEventListener("mousedown", mouseDownCallback);
    where.removeEventListener("mouseup", mouseUpCallback);
}


socket.onmessage = function(event) {
    var data = event.data;

    var atIdx = data.indexOf('@');
    if (atIdx === -1) {
        console.log("Server sent bad data: No @ symbol");
        return;
    }

    var type = data.slice(0, atIdx);

    var previousMath = currentMath;
    if (type === 'LaTeXBlock') {
        var box = document.createElement('textarea');
        box.className = 'latexoutputbox'
        var text = document.createTextNode(data.slice(atIdx + 1));
        box.appendChild(text);
        box.readOnly = true;

        // When it's clicked, select the whole thing
        $(box).on('focus', function(){this.select()});

        document.getElementById('repl').appendChild(box);

        // Needs to be set after it's in the real dom tree so that scrollHeight
        // is correct
        box.style.height = (box.scrollHeight)+"px";
    } else if (type === 'LaTeXLine') {
        var cm = currentCM;
        if (cm.getValue() === '') {
            putIntoCM(cm, "$ ", false);
        }
        var cursorPos = cm.getCursor();
        cm.replaceRange(data.slice(atIdx + 1), cursorPos);
        cm.focus();
    } else if (type === 'Err') {
        var errDiv = document.createElement('div');
        errDiv.className = 'output algebra-dsl-error';
        errDiv.innerHTML = data.slice(atIdx + 1);
        document.getElementById('repl').appendChild(errDiv);
    } else if (type === 'Math') {
        var nextAtIdx = data.indexOf('@', atIdx + 1);
        var nextNextAtIdx = data.indexOf('@', nextAtIdx + 1);

        var formulaNum = data.slice(atIdx + 1, nextAtIdx);
        var checkCheckbox = data.slice(nextAtIdx + 1, nextNextAtIdx) === 'true';

        var html = data.slice(nextNextAtIdx + 1);

        // Contains the checkbox, math, and buttons
        var fullDiv = document.createElement('div');
        fullDiv.className = 'output math-output';
        fullDiv.id = 'mathout'+formulaNum;
        fullDiv.setAttribute("formulanum", formulaNum);
        fullDiv.className += ' new-math-output';
        // Hide until the math is done typesetting
        fullDiv.style.display = 'none';

        var checkbox = document.createElement('input');
        checkbox.setAttribute('type', 'checkbox');
        fullDiv.appendChild(checkbox);
        checkbox.checked = checkCheckbox;
        // Add a bootstrap tooltip
        checkbox.setAttribute('data-toggle', 'tooltip');
        checkbox.setAttribute('data-placement', 'top');
        checkbox.setAttribute('title',
                "Select this equation for output");
        $(checkbox).tooltip();

        var mathBox = document.createElement('span');
        mathBox.id = 'formula'+formulaNum;
        mathBox.className += ' output disable-highlight';
        mathBox.innerHTML = html;
        fullDiv.appendChild(mathBox);

        createRecoverButtom(fullDiv);
        createGetCodeButton(fullDiv);

        document.getElementById('repl').appendChild(fullDiv);

        var cm = createCM();
        cm.prevOutput = fullDiv;
        currentCM = cm;

        currentMath = mathBox;

        // Handle Actual Formula
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, mathBox.id]);
        MathJax.Hub.Queue([onFinishTypesetting, mathBox, fullDiv, cm]);

        executeBatchCommands(cm);
    }

    if (!previousMath && (type === 'LaTeXBlock' || type === 'Err')) {
        var cm = createCM();
        putIntoCM(cm, "$ ", false);
        currentCM = cm;

        executeBatchCommands(cm);
    }
};

socket.onclose = function(event) {
    $('#connectionlostmodal').modal('show');
}

socket.onerror = function(event) {
    $('#connectionerrormodal').modal('show');
}

function createRecoverButtom(fullDiv) {
    var recoverButton = document.createElement('button');
    const recoverClass = "recover";
    recoverButton.innerHTML = "&#x27f2;";
    recoverButton.className += " " + recoverClass;
    // Add a bootstrap tooltip
    recoverButton.setAttribute('data-toggle', 'tooltip');
    recoverButton.setAttribute('data-placement', 'top');
    recoverButton.setAttribute('title',
            "Recover this equation");
    $(recoverButton).tooltip();


    recoverButton.addEventListener("click", function(e) {
        var formulaNum = e.target.parentNode.getAttribute("formulanum");
        var cm = currentCM;
        putIntoCM(cm, "recover " + formulaNum, true);
        sendToServer(cm);
    });
    fullDiv.appendChild(recoverButton);
}

function createGetCodeButton(fullDiv) {
    var getcodeButton = document.createElement('button');
    const recoverClass = "getcode";
    getcodeButton.innerHTML = "&lt;/&gt;";
    getcodeButton.className += " " + recoverClass;

    // Add a bootstrap tooltip
    getcodeButton.setAttribute('data-toggle', 'tooltip');
    getcodeButton.setAttribute('data-placement', 'top');
    getcodeButton.setAttribute('title',
            "Insert LaTeX code for this equation into current input");
    $(getcodeButton).tooltip();

    getcodeButton.addEventListener("click", function(e) {
        var formulaNum = e.target.parentNode.getAttribute("formulanum");
        socket.send("cmd@code " + formulaNum);
    });


    fullDiv.appendChild(getcodeButton);
}

function mathTreeNodeAbove(cur, topLevel) {
    while (!cur.hasAttribute('mathtreenode')) {
        if (cur === topLevel) {
            return null;
        }
        cur = cur.parentNode;
    }
    return cur;
}

function mathTreeNodeOrMultiparentAbove(cur, topLevel) {
    while (!cur.hasAttribute('mathtreenode') && !cur.hasAttribute('multiparent')) {
        if (cur === topLevel) {
            return null;
        }
        cur = cur.parentNode;
    }
    return cur;
}

function mathTreeNodeAboveBoth(n1, n2, topLevel) {
    var n1sAncestors = [n1];
    var cur = n1;
    while (true) {
        cur = mathTreeNodeAbove(cur.parentNode, topLevel);
        if (cur === null) {
            break;
        }
        n1sAncestors.push(cur);
    }

    cur = n2;

    while (n1sAncestors.indexOf(cur) == -1) {
        cur = mathTreeNodeAbove(cur.parentNode);
    }
    return cur;
}

function solidifySelection(cm) {
    cm = cm || currentCM;
    if (cm.selectedDOM) {
        cm.selectedDOM = null;
        cm.selectedTextMarker = null;
        replaceAllMathTags(cm);
    }
}

function onFinishTypesetting(element, fullDiv, nextcm) {
    // Remove any leftover MathML
    var maths = element.getElementsByTagName('math');
    while(maths.length > 0) {
        maths.item(0).remove();
    }

    fullDiv.style.display = 'block';

    nextcm.prevMath = element.cloneNode(true);

    // In case the user (or a batch command) types tags before we got done
    // typesetting it
    replaceAllMathTags(nextcm);

    currentCM.getWrapperElement().scrollIntoView({
        behavior: "smooth",
        block: "end",
    });
}

function replaceAllMathTags(cm, changeObj) {
    modified = true;
    if (!cm.prevMath) return;
    cm.autoSelectionChange = true;

    // TODO: Use changeObj intelligently to do less work
    // TODO: Keep colors consistent

    var prevOutput = $(cm.prevOutput).children(".output")[0];
    var prevMathBoxClone = cm.prevMath.cloneNode(true);
    currentMath = prevMathBoxClone;
    if (cm === currentCM) {
        addMathCallbacks(prevMathBoxClone);
    }

    var oldMarks = cm.getAllMarks();
    for (var i=0; i<oldMarks.length; i++) {
        oldMarks[i].clear();
    }

    var availColors = [9,8,7,6,5,4,3,2,1,0];

    var tagBeginStr = '#(mtn:';
    var tagBeginStrLen = tagBeginStr.length;

    var s = cm.getValue();
    var currentOffset = 0;
    var tagStartIdx;

    while ((tagStartIdx = s.indexOf(tagBeginStr, currentOffset)) != -1) {
        var tagEndIdx = s.indexOf(')', tagStartIdx+tagBeginStrLen);
        if (tagEndIdx == -1) {
            // There's no close-paren in the rest of the string, so there's no
            // more complete tags left
            break;
        }
        currentOffset = tagEndIdx;

        var tag = s.slice(tagStartIdx + tagBeginStrLen, tagEndIdx);
        var mathForTagAry = $(cm.prevMath).find("[mathtreenode='" + tag + "']");
        var mathInOutAry = $(prevMathBoxClone).find("[mathtreenode='" + tag + "']");
        if (mathForTagAry.length != 1 || mathInOutAry.length != 1) {
            console.log("No or >1 match for tag '" + tag + "': ");
            console.log(mathForTagAry);
            console.log(mathInOutAry);
            continue;
        }
        var color = availColors.pop();

        // Put the math in a span and highlight it
        var mathInOut = mathInOutAry[0];

        var tagStartPos = cm.posFromIndex(tagStartIdx);

        var elementToAddHoverTo;

        var changeAddedThisTag = changeObj
            && changeObj.origin === 'click'
            && tagStartPos.line === changeObj.from.line
            && tagStartPos.ch === changeObj.from.ch;

        if (changeAddedThisTag) {

            mathInOut.setAttribute('selected', 'true');
            mathInOut.setAttribute('highlighted', color);
            cm.selectedDOM = mathInOut;
            elementToAddHoverTo = mathInOut;
        } else {
            var newspan = document.createElement('span');
            newspan.setAttribute('highlighted', color);

            while (mathInOut.children.length > 0) {
                // This (re)moves the child
                newspan.appendChild(mathInOut.children[0]);
            }

            mathInOut.appendChild(newspan);
            elementToAddHoverTo = newspan;
        }

        // Replace the text with a copy of the math
        var mathCopy = mathForTagAry[0].cloneNode(true);

        var toInsert = document.createElement('span');
        toInsert.className = 'mjx-math mjx-chtml mathinequation';
        toInsert.appendChild(mathCopy);

        toInsert.setAttribute('highlighted', color);

        (function(themath) {
            toInsert.mouseEnterListener = function() {
                themath.setAttribute('hoverednode', true);
            }
            toInsert.addEventListener("mouseenter", toInsert.mouseEnterListener);

            toInsert.mouseLeaveListener = function() {
                themath.removeAttribute('hoverednode');
            }
            toInsert.addEventListener("mouseleave", toInsert.mouseLeaveListener);
        })(elementToAddHoverTo);

        var marker = cm.markText(
                tagStartPos,
                cm.posFromIndex(tagEndIdx+1),
                {replacedWith: toInsert});

        if (changeAddedThisTag) {
            cm.selectedTextMarker = marker;
        }
    }

    cm.prevOutput.replaceChild(prevMathBoxClone, prevOutput);
}

function executeBatchCommands(cm) {
    let cmd = batchCommands.shift();
    if (typeof cmd !== 'string') {
        return;
    }

    putIntoCM(cm, cmd, false);

    if (batchCommands.length > 0) {
        sendToServer(cm);
    }
}

function sendOutputLatex() {
    var tosend = "output ";

    var checkedBoxes = $('.math-output>input[type="checkbox"]:checked');

    var firstprint = true;
    for (var i=0; i<checkedBoxes.length; i++) {
        var fullDiv = checkedBoxes[i].parentNode;
        var id = fullDiv.getAttribute('formulanum');
        if (!firstprint) {
            tosend += ', ';
        }
        tosend += id;
        firstprint = false;
    }

    var cm = currentCM;
    putIntoCM(cm, tosend, true);
    sendToServer(cm);
}

function reclaimDown(cm) {
    var len = cm.history.length;
    //don't try this on the first box
    if (len < 2) {
        return;
    }
    cm.loc += -1;
    if (cm.loc < 0) {
        cm.loc = 0;
    } else {
        cm.history[len - 2 - cm.loc] = cm.getValue();

        putIntoCM(cm, cm.history[len - 1 - cm.loc], false);
    }
}

function reclaimUp(cm) {
    var len = cm.history.length;
    //don't try this on the first box
    if (len < 2) {
        return;
    }
    cm.loc += 1;
    if (cm.loc >= len){
        cm.loc = len - 1;
    } else {
        cm.history[len-cm.loc] = cm.getValue();
        putIntoCM(cm, cm.history[len - 1 - cm.loc], false);
    }
}

function sendFeedback() {
    var text = document.getElementById('feedbackText').value;
    var html = '<!DOCTYPE html>' + document.documentElement.outerHTML.replace(/ *\n */g, '');
    var msg = 'feedback@' + text + '\0' + html;

    socket.send(msg);

    document.getElementById('feedbackText').value = '';
}

function downloadSession() {
    var allBoxData = [];
    for (var i=0; i<CMhistory.length; i++) {
        allBoxData.push(CMhistory[i].getValue() + '\n');
    }

    var file = new Blob(allBoxData, {type: 'txt'});
    var defaultfilename = "khwarizmi-session.txt";

    modified = false;

    if (window.navigator.msSaveOrOpenBlob)
        // IE10+
        window.navigator.msSaveOrOpenBlob(file, defaultfilename);
    else {
        // Other browers
        var url = URL.createObjectURL(file);

        var a = document.createElement("a");
        a.href = url;
        a.download = defaultfilename;
        document.body.appendChild(a);

        a.click();
        setTimeout(function() {
            document.body.removeChild(a);
            window.URL.revokeObjectURL(url);
        }, 0);
    }
}

// Note: this function destroys what's in the current input box
function loadSession(file) {
    var fr = new FileReader();
    fr.onload = function(e) {
        var lines = e.target.result.split('\n');
        // The file ends with a newline: we don't want that
        var last = lines.pop();
        if (last.length > 0) {
            lines.push(last);
        }
        batchCommands = lines;
        executeBatchCommands(currentCM);
    };
    fr.readAsText(file);
}

// Give prompt unless user just saved
$(window).bind('beforeunload', function(){
    if (modified) {
      return "Are you sure you want to leave without saving changes?";
    }
    return undefined;
});
