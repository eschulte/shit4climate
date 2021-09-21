---
layout: index
title: Give a Shit for the Climate
---

<h1 class="s4c-only-small-phones">Shit 4 Climate</h1>
<h1 class="s4c-only-medium-phones">A Shit for Climate</h1>
<h1 class="s4c-only-large-phones">Give a Shit for Climate</h1>

Tired of spending your toilet time doom-scrolling environmental news?
Put your phone (and all those depressing facts your friends don't want
to hear) to good use!  Call your representative and give them some shit.

<form class="w3-container w3-margin">
Enter your <label>Zip Code:</label>
<input type="text" name="postal-code" id="postal-code" autocomplete="postal-code" onInput="zip_key_press()"></input>
</form>

<div class="w3-container w3-margin-top" id="call-senator-by-zip"></div>
<div class="w3-container w3-margin-top" id="call-representative-by-zip"></div>
<div class="w3-container w3-margin-top" id="call-state-representative-by-zip"></div>

We'll help with
[topics](#topics),
[scripts](#scripts), and
[questions](#questions).
So enter your zip, hit a phone number, put it on speaker, jump back
here, and let some poor congressional staffer hear you purge.

<div id="call-link"></div>

Topics
------

{% assign roll = collections.topics %}
{% include slider, roll: roll %}

Scripts
-------

{% assign roll = collections.scripts %}
{% include slider, roll: roll %}

Questions
---------

{% assign roll = collections.questions %}
{% include slider, roll: roll %}
