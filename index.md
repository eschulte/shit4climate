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

<!-- TODO: Instead back to one div and add each as a ul>li or something. -->
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
<div class="slides">
{% assign index = 0 %}
{% for topic in collections.topics %}
<div id="slide-{{ index }}" class="w3-container">
<a href="{{ topic.url| url }}">
<h3>{{ topic.data.title }}</h3>
<p class="w3-margin">{{ topic.data.brief|safe }}</p>
</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>

Scripts
-------

<div class="slider">
<div class="slides">
{% assign index = 0 %}
{% for script in collections.scripts %}
<div id="slide-{{ index }}" class="w3-container">
<a href="{{ script.url| url }}">
<h3>{{ script.data.title }}</h3>
<p class="w3-margin">{{ script.data.brief|safe }}</p>
</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>

Questions
---------

<div class="slider">
<div class="slides">
{% assign index = 0 %}
{% for question in collections.questions %}
<div id="slide-{{ index }}" class="w3-container">
<a href="{{ question.url| url }}">
<h3>{{ question.data.title }}</h3>
<p class="w3-margin">{{ question.data.brief|safe }}</p>
</a>
</div>
{% assign index = index| plus: 1 %}
{% endfor %}
</div>
</div>
