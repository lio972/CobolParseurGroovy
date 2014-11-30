
var completeQueryRunning = false;

jQuery(function() {
    jQuery("#breadCrumbLinks").jBreadCrumb();
    jQuery("#breadCrumbLinksSearch").jBreadCrumb();

    jQuery.fixSyntaxBlocks = function() {
        var i;
        var ccs = jQuery('div.codeContent');

        for (i = 0; i < ccs.length; i++) {
            var cc = jQuery(ccs[i]);
            var script = cc.find('script');
            var pre = jQuery('<pre/>');

            var content = script.html()
                .replace(/&amp;([a-zA-Z]+);/g, '&$1;')
                .replace(/&amp;#(\d+);/g, '&#$1;')
                .replace(/(<|&lt;)\!\[CDATA\[/g, '')
                .replace(/\]\](>|&gt;)/g, '')
            ;

            if (navigator.appName == 'Microsoft Internet Explorer') {
                content = content.substring(2).replace(/\n/g, '<br/>').replace(/ /g, '&nbsp;');
            }
            
            pre.html(content);
            cc.remove('script');
            cc.append(pre);
        }
    };
    jQuery.fixSyntaxBlocks();
    jQuery("pre").wrapInner("<code/>");
    hljs.initHighlighting();

    jQuery("body").bind("click", function (e) {
        jQuery('.dropdown-toggle, .menu').parent("li").removeClass("open");
    });
    jQuery(".dropdown-toggle, .menu").click(function (e) {
        jQuery('.dropdown-toggle, .menu').parent("li").removeClass("open");
        var $li = jQuery(this).parent("li").toggleClass('open');
        return false;
    });

    var dismiss = function() {
        jQuery("#pageLookup").hide();
        jQuery("#searchResults ul li").remove();
        key.setScope('all');
        jQuery("#quickSearch").val("").blur();
    };
    var stopPropagate = function(event) {
        if (event.preventDefault) event.preventDefault();
        if (event.stopPropagation) event.stopPropagation();
        if (event.cancelBubble) event.cancelBubble = true;
    };

    key('esc', 'quickSearchScope', function() {
        dismiss();
    });
    key('f', function(event) {
        stopPropagate(event);

        jQuery("#searchResults ul li").remove();
        jQuery("#pageLookup").show();
        jQuery("#quickSearch").focus();
        key.setScope('quickSearchScope');
        jQuery("#quickSearch").keydown(function(event) {
            var selected = null;
            if (event.keyCode == 27) {
                dismiss();
            } else if (event.keyCode == 13) {
                selected = jQuery("#searchResults ul li.selectedResult a");
                if (selected.size() > 0) {
                    window.location = selected.attr("href");
                }
            } else if (event.keyCode == 38) {
                stopPropagate(event);

                selected = jQuery("#searchResults ul li.selectedResult a");
                if (selected) {
                    var before = selected.parent().prev().children()[0];
                    if (before) {
                        selected.parent().removeClass("selectedResult");
                        selected = jQuery(before);
                        selected.parent().addClass("selectedResult");
                    }
                }
            } else if (event.keyCode == 40) {
                stopPropagate(event);

                selected = jQuery("#searchResults ul li.selectedResult a");
                if (selected) {
                    var after = selected.parent().next().children()[0];
                    if (after) {
                        selected.parent().removeClass("selectedResult");
                        selected = jQuery(after);
                        selected.parent().addClass("selectedResult");
                    }
                }
            } else {
                var elem = jQuery(this);
                if (elem.val().length > 2) {
                    if (!completeQueryRunning) {
                        completeQueryRunning = true;
                        setTimeout(function() {
                            completeQueryRunning = false;
                            var queryUrl = "http://groovy.codehaus.org/searchConfluence?spaceKey=GROOVY&query=" + encodeURI(elem.val());
                            jQuery.ajax({
                                url: queryUrl,
                                dataType: 'xml',
                                success: function(xml) {
                                    var links = {};
                                    jQuery(xml).find("result")
                                            .filter("[type='page']")
                                            .slice(0, 10).find("link")
                                            .filter("[type='text/html']")
                                            .each(function() {
                                                var pageName = jQuery(this).attr("href").match(".*/(.*)$")[1];
                                                links[pageName] = "http://groovy.codehaus.org/" + pageName;
                                            });
                                    console.log(links);
                                    jQuery("#searchResults ul li").remove();
                                    jQuery.each(links, function(key, value) {
                                        jQuery("<li><a href='" + value + "'>" + key.replace(/\+/g, ' ') + "</a></li>").appendTo("#searchResults ul");
                                    });
                                    jQuery("#searchResults ul li:first-child").addClass("selectedResult");
                                }
                            });
                        }, 1000);
                    }
                }
            }
        });

        jQuery("#pageLookup .close").click(function() {
            dismiss();
        })

        jQuery("#okButton").click(function() {
            var selected = jQuery("#searchResults ul li.selectedResult a");
            if (selected.size() > 0) {
                window.location = selected.attr("href");
            }

        })
    });

    key('s', function(event) {
        var field = jQuery("#search-input-field");
        field.focus();
        stopPropagate(event);
    });

    var hideSearch = function() {
        if (jQuery("#search-input-field").val().trim() == '') {
            jQuery("#search-area").hide();
            jQuery("#initial-page-content").show();
        }
    }
    jQuery("#search-input-field").keydown(function(event) {
        hideSearch();
        if (event.keyCode == 27) {
            jQuery(this).blur();
        }
    });
    jQuery("#search-input-field").blur(function() {
        hideSearch();
    });

});
