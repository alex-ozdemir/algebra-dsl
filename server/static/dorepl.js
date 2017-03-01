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
var CMhistory = []; //stores references to all the old CM instances
var isMousePressed = false;
var mousePressAnchor = null;

$(document).ready(function() {
    $('#feedbackModal').on('shown.bs.modal', function () {
        $('#feedbackText').focus();
    })

    var cm = createCM(null);

    cm.setValue("$ ");
    cm.setCursor({line: 0, ch: 2});
    currentCM = cm;

    document.addEventListener("mouseup", globalMouseUpCallback);
})

function createCM() {
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

    cm.on('keydown',  solidifyCurrent);
    cm.on('mousedown',  solidifyCurrent);
    return cm;
}

var socket = new WebSocket("ws://" + location.hostname + ':2794', "rust-websocket");

function sendToServer(cmBox) {
    cmBox.setOption('readOnly', true);
    cmBox.setOption('cursorBlinkRate', -1);
    socket.send("cmd@" + cmBox.getValue());
}

function addMathToCM(toAdd) {

    if (toAdd !== null) {
        //var copy = toAdd.cloneNode(true);

        //toAdd.setAttribute('highlighted',
        //    currentCM.availColors[currentCM.availColors.length - 1]);
        //toAdd.setAttribute('selected', 'true');

        //var subTreeNodes = copy.getElementsByTagName("*");
        //for (var i=0; i<subTreeNodes.length; i++) {
        //    subTreeNodes[i].removeAttribute('highlighted');
        //}

        //var toInsert = document.createElement('span');
        //toInsert.className = 'mjx-math mjx-chtml mathinequation';
        //toInsert.appendChild(copy);

        //toInsert.setAttribute('highlighted',
        //    currentCM.availColors[currentCM.availColors.length - 1]);

        //toInsert.mouseEnterListener = function() {
        //    toAdd.setAttribute('hoverednode', true);
        //}
        //toInsert.addEventListener("mouseenter", toInsert.mouseEnterListener);

        //toInsert.mouseLeaveListener = function() {
        //    toAdd.removeAttribute('hoverednode');
        //}
        //toInsert.addEventListener("mouseleave", toInsert.mouseLeaveListener);

        //var literaltext = '#(mtn:' + copy.getAttribute('mathtreenode') + ')';

        //var start_of_literal_loc = currentCM.getCursor();
        //currentCM.replaceRange(literaltext, start_of_literal_loc);
        //var end_of_literal_loc = currentCM.getCursor();

        //currentCM.selectedTextMarker = currentCM.markText(
        //    start_of_literal_loc, end_of_literal_loc,
        //    { replacedWith: toInsert });

        var start_of_literal_loc = currentCM.getCursor();
        var literaltext = '#(mtn:' + toAdd.getAttribute('mathtreenode') + ')';
        currentCM.replaceRange(literaltext, start_of_literal_loc);
        currentCM.selectedDOM = toAdd;
    }

    currentCM.focus();
}

function mouseClickCallback(event, mathBox) {
    var toAdd = mathTreeNodeAbove(event.target, mathBox);

    // Remove the other highlighted stuff
    if (currentCM.selectedDOM !== null) {
        var cur = event.target;
        while (cur !== mathBox && cur !== currentCM.selectedDOM) {
            cur = cur.parentNode;
        }

        if (cur === currentCM.selectedDOM) {
            currentCM.selectedDOM.removeAttribute('highlighted');

            // Delete the old stuff
            var oldfromto = currentCM.selectedTextMarker.find();
            currentCM.replaceRange('', oldfromto.from, oldfromto.to);

            // Re-click: expand!
            toAdd = mathTreeNodeAbove(currentCM.selectedDOM.parentNode, mathBox);

            if (toAdd === null) {
                currentCM.selectedDOM = null;
                currentCM.selectedTextMarker = null;
            }
        } else {
            // New click
            solidifyCurrent(currentCM.selectedDOM);
        }
    }
    addMathToCM(toAdd);
}

function mouseDownCallback(event) {
    isMousePressed = true;
    mousePressAnchor = mathTreeNodeAbove(event.target);
}

function mouseUpCallback(event) {
    if (mousePressAnchor !== null) {
        var mousePressHead = mathTreeNodeAbove(event.target);
        if (mousePressHead === mousePressAnchor) {
            mouseClickCallback(event, this);
        } else {
            solidifyCurrent();

            var parent = mathTreeNodeAboveBoth(mousePressAnchor, mousePressHead, this);
            addMathToCM(parent);
            solidifyCurrent();
        }
        mousePressAnchor = null;
    }
}

function globalMouseUpCallback(event) {
    isMousePressed = false;
    mousePressAnchor = null;
}

function addMathCallbacks(mathBox) {
    mathBox.addEventListener("mousedown", mouseDownCallback);
    mathBox.addEventListener("mouseup", mouseUpCallback);
}
function removeMathCallbacks(where) {
    where.removeEventListener("mousedown", mouseDownCallback);
    where.removeEventListener("mouseup", mouseUpCallback);
}


socket.onmessage = function(event) {
    var data = event.data;

    var codeMirrorInitialContents = '';

    var atIdx = data.indexOf('@');
    if (atIdx === -1) {
        console.log("Server sent bad data: No @ symbol");
        return;
    }
    var formulaNum = data.substr(0, atIdx);
    var rest = data.substr(atIdx+1);

    var atIdx = rest.indexOf('@');
    if (atIdx === -1) {
        console.log("Server sent bad data: Only one @ symbol");
        return;
    }
    var type = rest.substr(0, atIdx);
    var rest = rest.substr(atIdx+1);

    // Print out the latex or error or text response from server

    if (type === 'LaTeX') {
        var box = document.createElement('textarea');
        box.className = 'latexoutputbox'
        var text = document.createTextNode(rest);
        box.appendChild(text);
        box.readOnly = true;

        $(box).on('focus', function(){this.select()});
        document.getElementById('repl').appendChild(box);

        box.style.height = (box.scrollHeight)+"px";
    } else if (type === 'Err') {
        var errDiv = document.createElement('div');
        errDiv.className = 'output';

        errDiv.className += ' algebra-dsl-error';
        errDiv.innerHTML = rest;
        document.getElementById('repl').appendChild(errDiv);
    } else if (type === 'Re') {
      var reDiv = document.createElement('div');
      reDiv.className = 'output';
      reDiv.className += ' response-from-server';
      reDiv.innerHTML = rest;
      document.getElementById('repl').appendChild(reDiv);
    } else if (type === 'Input') {
        var atIdx = rest.indexOf('@');
        if (atIdx === -1) {
            console.log("Server sent bad data: Only two @ symbols in `Input`");
            return;
        }
        codeMirrorInitialContents = "$ " + rest.substr(0, atIdx);
        rest = rest.substr(atIdx + 1);
    }

    if (formulaNum > 0) {
        var prevFormula = document.getElementById('formula'+(formulaNum-1));
        if (prevFormula) {
            removeMathCallbacks(prevFormula);
        }
    }

    var prevMath = null;

    if (type === 'Math' || type === 'Input') {
        var fullDiv = document.createElement('div');
        fullDiv.className = 'output math-output';
        fullDiv.id = 'mathout'+formulaNum;
        document.getElementById('repl').appendChild(fullDiv);

        var checkbox = document.createElement('input');
        checkbox.setAttribute('type', 'checkbox');
        fullDiv.appendChild(checkbox);

        var mathBox = document.createElement('span');
        mathBox.id = 'formula'+formulaNum;
        mathBox.className += ' output disable-highlight';

        mathBox.innerHTML = rest;

        fullDiv.className += ' new-math-output';

        checkbox.checked = true;

        fullDiv.appendChild(mathBox);

        // Handle Actual Formula
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, mathBox.id]);
        MathJax.Hub.Queue([onFinishTypesetting, mathBox.id]);

        createRecoverButtom(fullDiv);
        createGetCodeButton(fullDiv);
    } else {
        var prevOut = document.getElementById('mathout'+(formulaNum-1));
        if (prevOut && (prevOut.childNodes[1].tagName.toLowerCase() == "span" && prevOut.childNodes.length > 1)) {
            var fullDiv = document.createElement('div');
            fullDiv.className = 'output math-output';
            fullDiv.id = 'mathout'+formulaNum;
            document.getElementById('repl').appendChild(fullDiv);

            var checkbox = document.createElement('input');
            checkbox.setAttribute('type', 'checkbox');
            fullDiv.appendChild(checkbox);

            var mathBox = prevOut.childNodes[1].cloneNode(true);
            mathBox.id = 'formula'+formulaNum;

            var subNodes = mathBox.getElementsByTagName('*');

            for (var i=0; i<subNodes.length; i++) {
                subNodes[i].removeAttribute('selected');
                subNodes[i].removeAttribute('highlighted');
            }

            addMathCallbacks(mathBox);
            prevMath = mathBox.cloneNode(true);

            fullDiv.appendChild(mathBox);

            createRecoverButtom(fullDiv);
            createGetCodeButton(fullDiv);
        } else {
            codeMirrorInitialContents = "$ ";
        }
    }

    var cm = createCM();
    cm.prevMath = prevMath;
    cm.setValue(codeMirrorInitialContents);
    cm.setCursor({line: 0, ch: codeMirrorInitialContents.length});
    cm.prevOutput = fullDiv;

    currentCM = cm;
};

function createRecoverButtom(fullDiv) {
    var recoverButton = document.createElement('button');
    const recoverClass = "recover";
    recoverButton.innerHTML = "&#x27f2;";
    recoverButton.className += " " + recoverClass;

    recoverButton.addEventListener("click", function(e) {
        var tosend = "recover "

        var cns = document.getElementById('repl').childNodes;

        var eqnIdx = -1;
        for (var i=0; i<cns.length; i++) {
            if (cns[i].classList.contains('new-math-output')) {
                eqnIdx++;
            }
            var buttonCandidates = cns[i].getElementsByClassName(recoverClass);
            if (buttonCandidates.length >= 2) {
                console.log("Too many " + recoverClass + " buttons!");
                console.log(buttonCandidates);
            }
            if (buttonCandidates.length > 0 && e.target === buttonCandidates[0] ) {
                tosend += eqnIdx;
                break;
            }
        }

        currentCM.setValue(tosend);

        sendToServer(currentCM);
    });
    fullDiv.appendChild(recoverButton);
}

function createGetCodeButton(fullDiv) {
    var getcodeButton = document.createElement('button');
    const recoverClass = "getcode";
    getcodeButton.innerHTML = "&lt;/&gt;";
    getcodeButton.className += " " + recoverClass;

    getcodeButton.addEventListener("click", function(e) {
        var tosend = "code "

        var cns = document.getElementById('repl').childNodes;

        var eqnIdx = -1;
        for (var i=0; i<cns.length; i++) {
            if (cns[i].classList.contains('new-math-output')) {
                eqnIdx++;
            }
            var buttonCandidates = cns[i].getElementsByClassName(recoverClass);
            if (buttonCandidates.length >= 2) {
                console.log("Too many " + recoverClass + " buttons!");
                console.log(buttonCandidates);
            }
            if (buttonCandidates.length > 0 && e.target === buttonCandidates[0] ) {
                tosend += eqnIdx;
                break;
            }
        }

        currentCM.setValue(tosend);

        sendToServer(currentCM);
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

function solidifyCurrent() {
    return;
    if (!currentCM.selectedDOM) {
        return;
    }
    currentCM.selectedDOM.removeAttribute('selected');

    var thisColor = currentCM.selectedDOM.getAttribute('highlighted');
    currentCM.selectedDOM.removeAttribute('highlighted');

    // Put a span inside that has a highlight color and has all of
    // curSelected's children
    var newspan = document.createElement('span');
    newspan.setAttribute('highlighted', thisColor);

    while (currentCM.selectedDOM.children.length > 0) {
        // This (re)moves the child from curSelected
        newspan.appendChild(currentCM.selectedDOM.children[0]);
    }

    currentCM.selectedDOM.appendChild(newspan);

    // Add listeners in case it's deleted
    (function(correspondingDOM, itscolor) {
        currentCM.selectedTextMarker.on('hide', function() {
            correspondingDOM.removeAttribute('selected');
            correspondingDOM.removeAttribute('highlighted');
        });
        currentCM.selectedTextMarker.on('unhide', function() {
            correspondingDOM.setAttribute('highlighted', itscolor);
        });

        var insertedSpan = currentCM.selectedTextMarker.replacedWith;

        insertedSpan.removeEventListener("mouseenter", insertedSpan.mouseEnterListener);
        insertedSpan.removeEventListener("mouseleave", insertedSpan.mouseLeaveListener);

        if (currentCM.selectedDOM.hasAttribute('hoverednode')) {
            currentCM.selectedDOM.removeAttribute('hoverednode');
            correspondingDOM.setAttribute('hoverednode', true);
        }

        insertedSpan.addEventListener("mouseenter", function() {
            correspondingDOM.setAttribute('hoverednode', true);
        });

        insertedSpan.addEventListener("mouseleave", function() {
            correspondingDOM.removeAttribute('hoverednode', true);
        });
    })(newspan, thisColor);


    //currentCM.availColors.pop();
    currentCM.selectedDOM = null;
    currentCM.selectedTextMarker = null;
}

function onFinishTypesetting(where) {
    var element = document.getElementById(where);

    // Remove any leftover MathML
    var maths = element.getElementsByTagName('math');
    while(maths.length > 0) {
        maths.item(0).remove();
    }

    addMathCallbacks(element);
    currentCM.prevMath = element.cloneNode(true);
}

function replaceAllMathTags(cm) {
    if (!cm.prevMath) return;

    var prevOutput = $(cm.prevOutput).children(".output")[0];
    var prevMathBoxClone = cm.prevMath.cloneNode(true);
    addMathCallbacks(prevMathBoxClone);
    cm.prevOutput.replaceChild(prevMathBoxClone, prevOutput);

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

        var newspan = document.createElement('span');
        newspan.setAttribute('highlighted', color);

        while (mathInOut.children.length > 0) {
            // This (re)moves the child
            newspan.appendChild(mathInOut.children[0]);
        }

        mathInOut.appendChild(newspan);

        // Replace the text with a copy of the math
        var mathCopy = mathForTagAry[0].cloneNode(true);

        var toInsert = document.createElement('span');
        toInsert.className = 'mjx-math mjx-chtml mathinequation';
        toInsert.appendChild(mathCopy);

        toInsert .setAttribute('highlighted', color);

        (function(themath) {
            toInsert.mouseEnterListener = function() {
                themath.setAttribute('hoverednode', true);
            }
            toInsert.addEventListener("mouseenter", toInsert.mouseEnterListener);

            toInsert.mouseLeaveListener = function() {
                themath.removeAttribute('hoverednode');
            }
            toInsert.addEventListener("mouseleave", toInsert.mouseLeaveListener);
        })(newspan);

        cm.selectedTextMarker = cm.markText(
                cm.posFromIndex(tagStartIdx),
                cm.posFromIndex(tagEndIdx+1),
                {replacedWith: toInsert});

        continue;






//        var full = s;
//        var tagTillEnd = s.slice(tagIdx + tagstartlen);
//
//        var endIdx = tagTillEnd.indexOf(')');
//        if (endIdx == -1) break;
//        var nextTagIdx = tagTillEnd.indexOf('#(mtn:');
//        if (nextTagIdx != -1 && nextTagIdx < endIdx) {
//            s = tagTillEnd.slice(nextTagIdx);
//            currentOffset += tagIdx + nextTagIdx + tagstartlen;
//            continue;
//        }
//
//        var tag = tagTillEnd.slice(0, endIdx);
//
//        // Find the tag in the math
//        var mathForTagAry = $(cm.prevOutput).find("[mathtreenode='" + tag + "']");
//        if (mathForTagAry.length != 1) {
//            s = tagTillEnd.slice(endIdx);
//            currentOffset += tagIdx + endIdx + tagstartlen;
//            continue;
//        }
//        var mathForTag = mathForTagAry[0];
//
//        var mathCopy = mathForTag.cloneNode(true);
//        mathCopy.removeAttribute('highlighted');
//        var mathCopyChildren = $(mathCopy).find("*");
//        for (var i=0; i<mathCopyChildren.length; i++) {
//            mathCopyChildren[i].removeAttribute('highlighted');
//        }
//
//        var toInsert = document.createElement('span');
//        toInsert.className = 'mjx-math mjx-chtml mathinequation';
//        toInsert.appendChild(mathCopy);
//
//        var color = availColors.pop();
//        mathForTag.setAttribute('highlighted', color);
//        toInsert  .setAttribute('highlighted', color);
//
//        (function(themath) {
//            toInsert.mouseEnterListener = function() {
//                themath.setAttribute('hoverednode', true);
//            }
//            toInsert.addEventListener("mouseenter", toInsert.mouseEnterListener);
//
//            toInsert.mouseLeaveListener = function() {
//                themath.removeAttribute('hoverednode');
//            }
//            toInsert.addEventListener("mouseleave", toInsert.mouseLeaveListener);
//        })(mathForTag);
//
//        cm.selectedTextMarker = cm.markText(
//                {line:0, ch:tagIdx + currentOffset},
//                {line:0, ch:tagIdx + currentOffset + endIdx + tagstartlen + 1},
//                {replacedWith: toInsert});
//
//        s = tagTillEnd.slice(endIdx);
//        currentOffset += tagIdx + endIdx + tagstartlen;
    }
}

function sendOutputLatex() {


    var tosend = "output ";

    var cns = document.getElementById('repl').childNodes;

    var firstPrint = true;
    var eqnIdx = -1;
    for (var i=0; i<cns.length; i++) {
        if (cns[i].classList.contains('new-math-output')) {
            eqnIdx++;
        }
        if (cns[i].classList.contains('math-output') && cns[i].childNodes.length > 1) {
            var checkbox = cns[i].childNodes[0];
            if (checkbox.checked) {
                if (!firstPrint) {
                    tosend += ', ';
                }
                tosend += '' + eqnIdx;
                firstPrint = false;
            }
        }
    }

    currentCM.setValue(tosend);

    sendToServer(currentCM);
}

function reclaimDown() {
    var len = currentCM.history.length;
    //don't try this on the first box
    if (len < 2) {
        return;
    }
    currentCM.loc += -1;
    if (currentCM.loc < 0) {
        currentCM.loc = 0;
    } else {
        currentCM.history[len-2-currentCM.loc] = currentCM.getValue();
        currentCM.setValue(currentCM.history[len-1-currentCM.loc]);
        currentCM.setCursor({line: 0, ch: currentCM.getValue().length});
    }
}

function reclaimUp() {
    var len = currentCM.history.length;
    //don't try this on the first box
    if (len < 2) {
        return;
    }
    currentCM.loc += 1;
    if (currentCM.loc >= len){
        currentCM.loc = len - 1;
    } else {
        currentCM.history[len-currentCM.loc] = currentCM.getValue();
        currentCM.setValue(currentCM.history[len-1-currentCM.loc]);
        currentCM.setCursor({line: 0, ch: currentCM.getValue().length});
    }
}

function sendFeedback() {
    var text = document.getElementById('feedbackText').value;
    var html = '<!DOCTYPE html>' + document.documentElement.outerHTML.replace(/ *\n */g, '');
    var msg = 'feedback@' + text + '\0' + html;

    socket.send(msg);

    document.getElementById('feedbackText').value = '';
}
