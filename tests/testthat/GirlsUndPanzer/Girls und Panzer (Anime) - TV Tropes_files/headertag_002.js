function splitIndexSlots(){for(var e=new Array,t=new Array,n=0;n<index_slots.length;n++){var i=index_slots[n].split("_"),d=i.shift(),a=d,s=i.join("_");"IOM"==a?e.push(s):"IPM"==a&&t.push(s)}return[e,t]}function isSameSlot(e,t){return e=e.split("_"),t=t.split("_"),e.pop()==t.pop()&&e.pop()==t.pop()}function clearTargeting(e){for(var t=[],n=[],i=[],d=splitIndexSlots(),a=d[0],s=d[1],o=0;o<a.length;o++)isSameSlot(a[o],e)||(t.push(a[o]),i.push("IOM_"+a[o]));for(var o=0;o<s.length;o++)isSameSlot(s[o],e)||(n.push(s[o]),i.push("IPM_"+s[o]));index_slots=i,a=t,s=n,googletag&&googletag.pubads&&googletag.pubads().setTargeting&&(a.length>0?googletag.pubads().setTargeting("IOM",a):googletag.pubads().setTargeting("IOM",[]),s.length>0?googletag.pubads().setTargeting("IPM",s):googletag.pubads().setTargeting("IPM",[]))}function cygnus_index_judge(e){for(var t={},n=/^(?:IOM|IPM)_(?:T([0-9])_)?(.*)_.*$/,i=[],d=0;d<e.length;d++)if("IPM"!==e[d].split("_")[0]){var a=n.exec(e[d]);if("undefined"!=typeof a&&null!==a){var s,o;if(s=a[1],o=a[2],("undefined"==typeof s||null===s)&&(s=0),t[o]=t[o]||{},"undefined"==typeof t[o].tier||t[o].tier<s){t[o].tier=s;var r=e[d];if(s>0){var _=e[d].split("_"),u=_.shift(),l=_.join("_");_.shift();var g=_.join("_");r=u+"_"+g,_IndexRequestData.targetIDToBid[g]=_IndexRequestData.targetIDToBid[l]}t[o].target=r}}}else i.push(e[d]);var p=[];for(var f in t)t.hasOwnProperty(f)!==!1&&p.push(t[f].target);return p.concat(i)}function cygnus_index_parse_res(e){try{if(e){if("object"!=typeof _IndexRequestData||"object"!=typeof _IndexRequestData.impIDToSlotID||"undefined"==typeof _IndexRequestData.impIDToSlotID[e.id])return;var t,n=0;"object"==typeof _IndexRequestData.reqOptions&&"object"==typeof _IndexRequestData.reqOptions[e.id]&&("function"==typeof _IndexRequestData.reqOptions[e.id].callback&&(t=_IndexRequestData.reqOptions[e.id].callback),"number"==typeof _IndexRequestData.reqOptions[e.id].targetMode&&(n=_IndexRequestData.reqOptions[e.id].targetMode)),_IndexRequestData.lastRequestID=e.id;for(var i=[],d="undefined"==typeof e.seatbid?0:e.seatbid.length,a=0;d>a;a++)for(var s=0;s<e.seatbid[a].bid.length;s++){var o=e.seatbid[a].bid[s];if("object"==typeof o.ext&&"string"==typeof o.ext.pricelevel&&"undefined"!=typeof _IndexRequestData.impIDToSlotID[e.id][o.impid]){var r=_IndexRequestData.impIDToSlotID[e.id][o.impid];"undefined"==typeof index_slots&&(index_slots=[]),"undefined"==typeof _IndexRequestData.targetIDToBid&&(_IndexRequestData.targetIDToBid={});var _,u;"string"==typeof o.ext.dealid?(_=1===n?r+o.ext.pricelevel:r+"_"+o.ext.dealid,u="IPM_"):(_=r+o.ext.pricelevel,u="IOM_"),index_slots.push(u+_),void 0===_IndexRequestData.targetIDToBid[_]?_IndexRequestData.targetIDToBid[_]=[o.adm]:_IndexRequestData.targetIDToBid[_].push(o.adm);var l={};l.impressionID=o.impid,"undefined"!=typeof o.ext.dealid&&(l.dealID=o.ext.dealid),l.bid=o.price,l.slotID=r,l.priceLevel=o.ext.pricelevel,l.target=u+_,i.push(l)}}"function"==typeof t&&(0===i.length?t(e.id):t(e.id,i)),index_slots=cygnus_index_judge(index_slots)}}catch(g){}"undefined"==typeof index_slots&&(index_slots=[]),cygnus_index_set_targets()}function cygnus_index_set_targets(){"function"==typeof window.cygnus_index_ready_state&&window.cygnus_index_ready_state()}function cygnus_log(e){}function index_render(e,t){"undefined"==typeof index_demand&&(index_demand={});for(var n=t.split("_"),i=n[0],d=0;d<index_slots_render.length;d++)if("string"==typeof index_slots_render[d]["IOM_"+t]){cygnus_log("unpack tier"+t),t=index_slots_render[d]["IOM_"+t];var a=t.split("_");t=a[1]+"_"+a[2]+"_"+a[3],i=a[1]+"_"+a[2]}cygnus_log("index_render: "+t);try{var s=_IndexRequestData.targetIDToBid[t].pop();null!=s&&(e.write(s),cygnus_log("Logged demand for slot "+i),index_demand[i]=1,delete index_no_demand[i])}catch(o){}}function cygnus_copy(e){var t=new Object;for(var n in e)e.hasOwnProperty(n)&&(t[n]=e[n]);return t}function getSlotInfo(e){for(var t=0;t<cygnus_index_args.slots.length;t++){var n=cygnus_index_args.slots[t];if(n.id==e)return n}}index_slot_to_size={},index_slots_render=[],index_slots_add=[];var cygnus_tid=4;
;window.proper_6bc1e9e7_8778767f_5({"id": "380617940"});