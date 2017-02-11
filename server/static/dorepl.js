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
    }
});

var currentCM = null;

window.onload = function() {
    createCM();

    currentCM.setValue("$ ");
    currentCM.setCursor({line: 0, ch: 2});
}

function createCM() {
    currentCM = CodeMirror(document.getElementById('repl'), {
        autofocus: true,
        extraKeys: {
            'Enter': sendToServer,
        },
    });

    // Use last colors first
    currentCM.availColors = [9,8,7,6,5,4,3,2,1,0];
    currentCM.selectedDOM = null;
    currentCM.selectedTextMarker = null;

    currentCM.on('keydown',  solidifyCurrent);
    currentCM.on('mousedown',  solidifyCurrent);
}

var socket = new WebSocket("ws://" + location.hostname + ':2794', "rust-websocket");

function sendToServer(cmBox) {
    currentCM.setOption('readOnly', true);
    currentCM.setOption('cursorBlinkRate', -1);
    socket.send(cmBox.getValue());
}

socket.onmessage = function(event) {
    var data = event.data;

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

    // Print out the latex or error

    if (type === 'LaTeX') {
        var CM = CodeMirror(document.getElementById('repl'),
                            {readOnly: true, cursorBlinkRate: -1});

        CM.setValue(rest);
    } else if (type === 'Err') {
        var errDiv = document.createElement('div');
        errDiv.className = 'output';

        errDiv.className += ' algebra-dsl-error';
        errDiv.innerHTML = rest;
        document.getElementById('repl').appendChild(errDiv);
    }

    // Print the current math regardless

    var fullDiv = document.createElement('div');
    fullDiv.className = 'output math-output';
    fullDiv.id = 'mathout'+formulaNum;

    document.getElementById('repl').appendChild(fullDiv);

    if (formulaNum > 0) {
        document.getElementById('mathout'+(formulaNum-1))
                    .removeEventListener("click", clickMathCallback);
    }

    var checkbox = document.createElement('input');
    checkbox.setAttribute('type', 'checkbox');
    //checkbox.style = "float: left;";

    fullDiv.appendChild(checkbox);

    if (type === 'Math') {
        var mathBox = document.createElement('span');
        mathBox.id = 'formula'+formulaNum;
        mathBox.className += ' output disable-highlight';

        mathBox.innerHTML = rest;

        fullDiv.className += ' new-math-output';

        checkbox.checked = true;

        fullDiv.appendChild(mathBox);

        // Handle Actual Formula
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, mathBox.id]);
        MathJax.Hub.Queue([onFinishTypesetting, fullDiv.id]);
    } else {
        var prevOut = document.getElementById('mathout'+(formulaNum-1));
        if (prevOut && prevOut.childNodes.length > 1) {
            var mathBox = prevOut.childNodes[1].cloneNode(true);
            var subNodes = mathBox.getElementsByTagName('*');

            for (var i=0; i<subNodes.length; i++) {
                subNodes[i].removeAttribute('selected');
                subNodes[i].removeAttribute('highlighted');
            }

            mathBox.addEventListener("click", clickMathCallback, false);

            fullDiv.appendChild(mathBox);
        }
    }

    createCM();
};

function mathTreeNodeNodeAbove(cur, topLevel) {
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
    while (cur != topLevel) {
        cur = cur.parentNode;
        n1sAncestors.push(cur);
    }

    cur = n2;

    while (n1sAncestors.indexOf(cur) == -1) {
        cur = cur.parentNode;
    }
    return cur;
}

function solidifyCurrent() {
    if (currentCM.selectedDOM) {
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


        currentCM.availColors.pop();
        currentCM.selectedDOM = null;
        currentCM.selectedTextMarker = null;
    }
}

function clickMathCallback(event) {
    var toAdd = mathTreeNodeNodeAbove(event.target, this);

    // Remove the other highlighted stuff
    if (currentCM.selectedDOM !== null) {
        var cur = event.target;
        while (cur !== this && cur !== currentCM.selectedDOM) {
            cur = cur.parentNode;
        }

        if (cur === currentCM.selectedDOM) {
            currentCM.selectedDOM.removeAttribute('selected');
            currentCM.selectedDOM.removeAttribute('highlighted');

            // Delete the old stuff
            var oldfromto = currentCM.selectedTextMarker.find();
            currentCM.replaceRange('', oldfromto.from, oldfromto.to);

            // Re-click: expand!
            toAdd = mathTreeNodeNodeAbove(currentCM.selectedDOM.parentNode, this);

            if (toAdd === null) {
                currentCM.selectedDOM = null;
                currentCM.selectedTextMarker = null;
            }
        } else {
            // New click
            solidifyCurrent(currentCM.selectedDOM);
        }
    }

    if (toAdd !== null) {
        var copy = toAdd.cloneNode(true);

        toAdd.setAttribute('highlighted',
            currentCM.availColors[currentCM.availColors.length - 1]);
        toAdd.setAttribute('selected', 'true');

        var subTreeNodes = copy.getElementsByTagName("*");
        for (var i=0; i<subTreeNodes.length; i++) {
            subTreeNodes[i].removeAttribute('highlighted');
        }

        var inside = document.createElement('span');
        inside.className = 'mjx-math mjx-chtml mathInEquation';
        inside.appendChild(copy);

        var toInsert = document.createElement('span');
        toInsert.appendChild(inside);

        toInsert.setAttribute('highlighted',
            currentCM.availColors[currentCM.availColors.length - 1]);

        toInsert.mouseEnterListener = function() {
            toAdd.setAttribute('hoverednode', true);
        }
        toInsert.addEventListener("mouseenter", toInsert.mouseEnterListener);

        toInsert.mouseLeaveListener = function() {
            toAdd.removeAttribute('hoverednode');
        }
        toInsert.addEventListener("mouseleave", toInsert.mouseLeaveListener);

        var literaltext = '#(mtn:' + copy.getAttribute('mathtreenode') + ')';

        var start_of_literal_loc = currentCM.getCursor();
        currentCM.replaceRange(literaltext, start_of_literal_loc);
        var end_of_literal_loc = currentCM.getCursor();

        currentCM.selectedTextMarker = currentCM.markText(
            start_of_literal_loc, end_of_literal_loc,
            { replacedWith: toInsert });
        currentCM.selectedDOM = toAdd;
    }

    currentCM.focus();
}

function onFinishTypesetting(where) {
    var element = document.getElementById(where);

    // Remove any leftover MathML
    var maths = element.getElementsByTagName('math');
    while(maths.length > 0) {
        maths.item(0).remove();
    }

    element.addEventListener("click", clickMathCallback, false);

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
        if (cns[i].classList.contains('math-output')) {
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
