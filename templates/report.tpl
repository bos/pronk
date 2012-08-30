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
   </tr>
   <tr>
    <td></td>
    <td><div id="thru0" class="timechart"
             style="width:450px;height:278px;"></div></td>
   </tr>
   <tr>
    <td></td>
    <td><div id="wtf0" class="timechart"
             style="width:450px;height:278px;"></div></td>
   </tr>
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
  function mangulate(number, name, mean, times, lats, kdetimes,
                     kdepdf, thrTimes, thrValues) {
    var meanSecs = mean;
    var units = $.timeUnits(mean);
    var scale = units[0];
    units = units[1];
    mean *= scale;
    kdetimes = $.scaleBy(scale, kdetimes);
    lats = $.scaleBy(scale, lats);
    var kq = $("#kde" + number);
    var ymin = Math.min(kdetimes[0], Math.min.apply(undefined, lats));
    var ymax = Math.max(kdetimes[kdetimes.length-1], Math.max.apply(undefined, lats));
    var k = $.plot(kq,
           [{ label: name + " latency densities",
              data: $.zip(kdepdf, kdetimes),
              }],
           { yaxis: { min: ymin, max: ymax, labelWidth: 50,
                      tickFormatter: $.unitFormatter(units) },
             xaxis: { ticks: false },
             grid: { hoverable: true, markings: [ { color: '#6fd3fb',
                     lineWidth: 1.5, yaxis: { from: mean, to: mean } } ] },
           });
    var o = k.pointOffset({ y: mean, x: 0 });
    kq.append('<div class="meanlegend" title="' + $.renderTime(meanSecs) +
              '" style="position:absolute;top:' + (o.top + 4) +
              'px;left:139px;">mean</div>');
    $.plot($("#time" + number),
           [{ label: name + " latencies",
              data: $.zip(times, lats) }],
           { points: { show: true, radius: 2 },
             grid: { hoverable: true },
             yaxis: { min: ymin, max: ymax, labelWidth: 50,
                      tickFormatter: $.unitFormatter(units) },
             xaxis: { min: 0, max: thrTimes[thrTimes.length-1],
                      ticks: false },
           });
    $.plot($("#thru" + number),
           [{ label: name + " rps",
              data: $.zip(thrTimes,$.scaleBy(1/(thrTimes[1]-thrTimes[0]), thrValues)) }],
           { bars: { show: true, barWidth: thrTimes[1]-thrTimes[0] },
             grid: { hoverable: true },
             yaxis: { labelWidth: 50, tickFormatter: $.unitFormatter("reqs") },
             xaxis: { ticks: false },
           });
    var x = $.scaleBy(1/(thrTimes[1]-thrTimes[0]), thrValues);
    var y = $.scaleBy(1, lats);
    x.sort(function(a,b){return a-b;});
    y.sort(function(a,b){return a-b;});
    $.plot($("#wtf" + number),
           [{ label: name + " rps",
              data: $.zip(x,y) }],
           { lines: { show: true }});
    $.addTooltip("#kde" + number, function(x,y) { return y + ' ' + units; });
    $.addTooltip("#time" + number, function(x,y) {
      return 'Latency at ' + $.renderTime(x) + ': ' + y + ' ' + units;
    });
    $.addTooltip("#thru" + number, function(x,y) {
      return 'Req/sec at ' + $.renderTime(x) + ': ' + parseInt(y);
    });
  };
  mangulate(0, "name",
            {{latency.mean}},
            /* start */ [{{#latValues}}{{summStart}},{{/latValues}}],
            /* elapsed */ [{{#latValues}}{{summElapsed}},{{/latValues}}],
            /* kde times */ [{{#latKdeTimes}}{{x}},{{/latKdeTimes}}],
            /* kde pdf */ [{{#latKdePDF}}{{x}},{{/latKdePDF}}],
            /* thr times */ [{{#thrTimes}}{{x}},{{/thrTimes}}],
            /* thr values */ [{{#thrValues}}{{x}},{{/thrValues}}]);
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
