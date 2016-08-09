

# Module sc_scimp_util #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Utility functions to decode SCIMP elements embedded in
message stanzas.

Copyright (c) 2012,2013 Silent Circle LLC

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-json_term">json_term()</a> ###


<pre><code>
json_term() = [{binary(), <a href="#type-json_term">json_term()</a>}] | [<a href="#type-json_term">json_term()</a>] | true | false | null | integer() | float() | binary()
</code></pre>

====================================================================
API
====================================================================



### <a name="type-scimp_op">scimp_op()</a> ###


<pre><code>
scimp_op() = data | commit | confirm | dh1 | dh2
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_scimp_op-1">get_scimp_op/1</a></td><td></td></tr><tr><td valign="top"><a href="#msg_to_json-1">msg_to_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#scimp_ops-0">scimp_ops/0</a></td><td></td></tr><tr><td valign="top"><a href="#scimp_type-1">scimp_type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_scimp_op-1"></a>

### get_scimp_op/1 ###

<pre><code>
get_scimp_op(JSON::<a href="#type-json_term">json_term()</a> | undefined) -&gt; <a href="#type-scimp_op">scimp_op()</a> | undefined
</code></pre>
<br />

<a name="msg_to_json-1"></a>

### msg_to_json/1 ###

<pre><code>
msg_to_json(Xmlelement::#xmlelement{}) -&gt; <a href="#type-json_term">json_term()</a> | undefined
</code></pre>
<br />

<a name="scimp_ops-0"></a>

### scimp_ops/0 ###

<pre><code>
scimp_ops() -&gt; [{binary(), atom()}]
</code></pre>
<br />

<a name="scimp_type-1"></a>

### scimp_type/1 ###

<pre><code>
scimp_type(Xmlelement::#xmlelement{}) -&gt; <a href="#type-scimp_op">scimp_op()</a> | undefined
</code></pre>
<br />

