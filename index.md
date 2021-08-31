---
layout: index
title: Give a Shit for the Climate
---

<h1 class="s4c-only-small-phones">Shit 4 Climate</h1>
<h1 class="s4c-only-medium-phones">A Shit for Climate</h1>
<h1 class="s4c-only-large-phones">Give a Shit for the Climate</h1>

Tired of spending your toilet time doom-scrolling environmental news?
Put your phone (and all those depressing facts your friends don't want
to hear) to good use!  Call your representative and give them a
piece of your mind.

<form class="w3-container w3-margin">
Enter your <label>Postal Code:</label>
<input type="text" name="postal-code" id="postal-code" autocomplete="postal-code" onInput="zip_key_press()"></input>
</form>

<div class="w3-container w3-margin" id="call-representative-by-zip"></div>
<div class="w3-container w3-margin" id="call-senator-by-zip"></div>

We'll help make your call productive with
[topics](#topics),
[scripts](#scripts), and
[questions](#questions).

<div id="call-link"></div>

Topics
------

<div class="slider">
{% assign index = 0 %}
{% for topic in collections.topics %}
<a href="#slide-{{ index }}">{{ index }}</a>{% assign index = index| plus: 1 %}{% endfor %}
<div class="slides">
{% assign index = 0 %}
{% for topic in collections.topics %}
<div id="slide-{{ index }}">
<a href="{{ topic.url| url }}">{{ topic.data.title }}</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>

Scripts
-------

<div class="slider">
{% assign index = 0 %}
{% for script in collections.scripts %}
<a href="#slide-{{ index }}">{{ index }}</a>{% assign index = index| plus: 1 %}{% endfor %}
<div class="slides">
{% assign index = 0 %}
{% for script in collections.scripts %}
<div id="slide-{{ index }}">
<a href="{{ script.url| url }}">{{ script.data.title }}</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>

Questions
---------

<div class="slider">
{% assign index = 0 %}
{% for question in collections.questions %}
<a href="#slide-{{ index }}">{{ index }}</a>{% assign index = index| plus: 1 %}{% endfor %}
<div class="slides">
{% assign index = 0 %}
{% for question in collections.questions %}
<div id="slide-{{ index }}">
<a href="{{ question.url| url }}">{{ question.data.title }}</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>
