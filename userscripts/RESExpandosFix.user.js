// ==UserScript==
// @name RESExpandosFix
// @author MonDieu!
// @include http://reddit.com/*
// @include https://reddit.com/*
// @include https://*reddit.com/*
// @include http://*reddit.com/*
// @require https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js
// @grant GM_addStyle
// @version 0.01
// ==/UserScript==
$(document).ready(function()
		{
			$("a.expando-button").on('click',function()
				{
					// do nothing
					0;
				});
		});
