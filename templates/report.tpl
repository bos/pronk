<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>pronk performance analysis</title>
    <!--[if lte IE 8]>
      <script language="javascript" type="text/javascript">
        {{#include}}js/excanvas-r3.min.js{{/include}}
      </script>
    <![endif]-->
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery-1.6.4.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.flot-0.7.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.criterion.js{{/include}}
    </script>
    <style type="text/css">
{{#include}}criterion.css{{/include}}
</style>
 </head>
    <body>
      <div class="body">
    <h1>pronk performance analysis</h1>

<h2><a name="b{{number}}">name</a></h2>
 <table width="100%">
  <tbody>
   <tr>
    <td><div id="kde0" class="kdechart"
             style="width:450px;height:278px;"></div></td>
    <td><div id="time0" class="timechart"
             style="width:450px;height:278px;"></div></td>
  </tbody>
 </table>

 <span class="outliers">
   <p>Outlying measurements have {{anOutlierVar.ovDesc}}
     (<span class="percent">{{anOutlierVar.ovFraction}}</span>%)
     effect on estimated standard deviation.</p>
 </span>
{{/report}}

<script type="text/javascript">
$(function () {
  function mangulate(number, name, mean, times, lats, kdetimes, kdepdf) {
    var meanSecs = mean;
    var units = $.timeUnits(mean);
    var scale = units[0];
    units = units[1];
    mean *= scale;
    kdetimes = $.scaleBy(scale, kdetimes);
    lats = $.scaleBy(scale, lats);
    var kq = $("#kde" + number);
    var xmin = Math.min(kdetimes[0], Math.min.apply(undefined, lats));
    var xmax = Math.max(kdetimes[kdetimes.length-1], Math.max.apply(undefined, lats));
    var k = $.plot(kq,
           [{ label: name + " latency densities",
              data: $.zip(kdetimes, kdepdf),
              }],
           { xaxis: { min: xmin, max: xmax,
                      tickFormatter: $.unitFormatter(units) },
             yaxis: { ticks: false },
             grid: { hoverable: true, markings: [ { color: '#6fd3fb',
                     lineWidth: 1.5, xaxis: { from: mean, to: mean } } ] },
           });
    var o = k.pointOffset({ x: mean, y: 0});
    kq.append('<div class="meanlegend" title="' + $.renderTime(meanSecs) +
              '" style="position:absolute;left:' + (o.left + 4) +
              'px;bottom:139px;">mean</div>');
    $.plot($("#time" + number),
           [{ label: name + " latencies",
              data: $.zip(lats, times) }],
           { points: { show: true, radius: 2 },
             grid: { hoverable: true },
             xaxis: { min: xmin, max: xmax,
                      tickFormatter: $.unitFormatter(units) },
             yaxis: { min: 0, max: times[times.length-1],
                      ticks: false,
                      transform: function(v) { return -v; },
                      inverseTransform: function(v) { return -v; } },
           });
    $.addTooltip("#kde" + number, function(x,y) { return x + ' ' + units; });
    $.addTooltip("#time" + number, function(x,y) {
      return 'Latency at ' + $.renderTime(y) + ': ' + x + ' ' + units;
    });
  };
  mangulate(0, "name",
            {{latency.mean}},
            /* start */ [{{#latValues}}{{summStart}},{{/latValues}}],
            /* elapsed */ [{{#latValues}}{{summElapsed}},{{/latValues}}],
            /* kde times */ [{{#latKdeTimes}}{{x}},{{/latKdeTimes}}],
            /* kde pdf */ [{{#latKdePDF}}{{x}},{{/latKdePDF}}]);
});
$(document).ready(function () {
    $(".time").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".citime").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".percent").text(function(_, text) {
        return (text*100).toFixed(1);
      });
  });
</script>

   </div>
 </body>
</html>
