// Place your application-specific JavaScript functions and classes here
// This file is automatically included by javascript_include_tag :defaults
function toggle_blind(item_id) {
    if (Element.visible(item_id)) {
//        new Effect.BlindUp(item_id,{});
        Element.hide(item_id);
    } else {
//        new Effect.BlindDown(item_id,{});
        Element.show(item_id);                
    }
}

function expand(solution_id) {
    hide("expand_solution" + solution_id)
    hide("brief_solution" + solution_id)
    show("contract_solution" + solution_id)
    show("full_solution" + solution_id)
}

function contract(solution_id) {
    hide("contract_solution" + solution_id)
    hide("full_solution" + solution_id)
    show("expand_solution" + solution_id)
    show("brief_solution" + solution_id)
}

function hide(element) {
    document.getElementById(element).style.display='none';
}

function show(element) {
    document.getElementById(element).style.display='';
}

function open(item_id) {
    if (Element.visible(item_id)) {
        for(item in Element.immediateDescendants(item_id)) {
            open(item)
        }
    } else {
        Element.show(item_id)
    }
}

function close(item_id) {
    Element.hide(item_id)
}

function indentify(e) {
    e = e || window.event
    ch = e.which || e.keyCode
    keychar = String.fromCharCode(ch)
    if (ch==13) {
        return false
    }
    return true    
}

function slugify(full_text, slug) {
    document.getElementById(slug).value = full_text.toLowerCase().replace(/ /g,'-').replace(/[^A-Za-z0-9-]+/g,'');
}

function indent(el) {
    	var replaceSelection = function(el, text) {
		if (el.setSelectionRange) {
			var s = el.selectionStart, e = el.selectionEnd;
			var t = el.scrollTop, l = el.scrollLeft;
			var oldlength = el.value.length;
			el.value = el.value.substr(0, s) + text + el.value.substr(e);
			s += el.value.length - oldlength;
			el.setSelectionRange(s, s);
			el.scrollTop = t;
			el.scrollLeft = l;
			return true;
		} else if (document.selection) {
			var sel = document.selection.createRange();
			var b = sel.duplicate();
			sel.text = text;
			sel.select();
			return true;
		}
		
		return false;
	};

	var selectionContains =
		(el.setSelectionRange ? function(el, text) { return el.value.substring(el.selectionStart, el.selectionEnd).indexOf(text) != -1; } :
		(document.selection ? function(el, text) { return document.selection.createRange().text.indexOf(text) != -1; } :
		function() { return false; }));

	var selectionMatches =
		(el.setSelectionRange ? function(el, re) { return re.test(el.value.substring(el.selectionStart, el.selectionEnd)); } :
		(document.selection ? function(el, re) { return re.test(document.selection.createRange().text); } :
		function() { return false; }));

	var getCaretPosition =
		(document.selection ? function(el) {
				var sel = document.selection.createRange();
				var b = sel.duplicate();
				var temp = sel.text;
				sel.text = "\xff";
				sel.setEndPoint("StartToStart", b);
				var pos = el.value.search("\xff");
				sel.select();
				sel.text = temp;
				sel.setEndPoint("StartToStart", b);
				sel.select();
				return pos;
			} :
		(el.selectionStart || el.selectionStart == '0' ? function(el) {
				return el.selectionStart;
			} :
		function() { return -1; }));

	var getTextSize = function(text) {
			var span = document.createElement("span");
			span.style.visibility.hidden = true;
			pre.appendChild(span);
			span.appendChild(document.createTextNode(text));
			span.appendChild(document.createElement("br"));
			var width = span.offsetWidth;
			var height = span.offsetHeight;
			span.parentNode.removeChild(span);
			
			return { width: width, height: height };
	};
        
	el.onkeypress = function(e) {
		e = e || window.event;
                ch = e.which || e.keyCode
		if (ch == 9) {
			if (!selectionContains(el, "\n"))
				return !replaceSelection(el, "\t");
			else
				return false;
		} else if (ch == 13) {
			var p = getCaretPosition(el);

			if (p != -1) {
			var v = el.value, add = "", addIndent = 0;

				for (i = p - 1; i >= 0 && v.charAt(i) != "\n"; --i) {
					var c = v.charAt(i);

					if (c == "\t" || c == " ")
						add = c + add;
					else
						add = "";
				}
				
				return !replaceSelection(el, "\n" + add);
			}
		} 

		//window.status = e.keyCode
	}
        
}