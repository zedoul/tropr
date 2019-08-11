    var edgeSupport_281685 = "f";
    var isIEBrowser_281685=false;
    var browserVersion_281685;

    function initiateNewRequest_281685(edgeSupport, html5Support) {
            var newUrl = "https://ads.everesttech.net/ads/mts/15979/4249?DFA_Click_Tracker=https%3A%2F%2Fadclick.g.doubleclick.net%2Fpcs%2Fclick%3Fxai%3DAKAOjstVOJBoenJ4q08upWgqaDSf4012xdnYR5Xec9u6vIhHsihJxit2sSpAxfsEY5b3V01GWj73vONlEOhvfXBuYJxKTpMpI3pmvidngjfL767PJFfgimSi6rcOYvixl9IqbHbzfMMH0RXyICJ1cd36pbVXRosmW-lDGdwZ3uQOXzIlCNh6U02ctBx9izyh_NIbUY0qNOLNA-P_jGh7OlIwoQUhOuzNPxFNzGZ2Tz36KRb6Qjk6HalRVBBdIZcpMLoP85iHH95BdEEj7-0nlS7WIpedvN2t7z4FcMfavzTnZEDhadWV_5eurczgr9e3cOuXbZqjb4MhpP_qfWOq1sNQ-C36ABSp2zBbCuveJpK3i2sYdxoIEcyzHIuwolPwDfvNT0hXNGd46b-hMdtz1AEuOukkroqUR1D2giNXnZEPXf1d63maKRud8C-W9KIaDsPTvaK9QLfPykbtjEmN8GnCcYLza5XMOnUcD_Mb3iC1kZ2nHhMiHQFj9hssCFFmRcmCD2sTYLrLaFHxksJBYyscCjZqlOm01i19dKn-KZsEKtDQz8qdJIu6yMRy7huGj9DTCgXoEax-klFbxT0FN1hhIUg6CY4CEuwmnqc33vi6EI0DtX0s3NG5ZaYWoIxYgsjfYaxWcBVcOYGR1d8ufKuzzfDnzCW5SmyPCUtFwY0H2m47hrVebGyOeWYq2_0Qd8bmb3UJxbEPmnlPNDUt6DerMcSk6UM-_tRMAvkt8PSU6VRS93qDGGVNtWQQIrGriuG-HT2oxLnCQYSqCotr7qqnshQXfXUHANQTX5NQQP4dzNgy87hldmLPVxjMcd9hcLIw_43WMgqvcZgISBZymxZKpwwzs3UCkiHfIHJjzDgJDdkz2GvAo_hYJmI5OxHEr9cwBmqvn6NToAUykj23earlcMSF1bUpiLn0DGO3c3VAnFiYw16r4AXzNhDfeF2PuiYulXVPFnp_izzLK7OvIbrTNLt2vNjK_d5SEkYHm4OQOfe53FVz8Ra3cY2UZPa63gcO58nQ1e_IaKHMdDRqlWqJtIt44wEDuBMvkhRWtQU%26sai%3DAMfl-YSesLOyj0g_8B4sCJk5mtl9eVxq_QR1EbcZR2TMmMiRUwK6oBzaFlrTPScApXu8lIqh519l2bRKNQMqfEvJrK79HVRILIPRO-aAu2LX6ReHWS9yBannco09T6QjoEEJbAu7Sw6T3lXUdXcuDgiBf2GSAwTpe-G3nrsdKjdFBUY42b0%26sig%3DCg0ArKJSzCoFmQKkyPqdEAE%26urlfix%3D1%26adurl%3D&DFA_BuyId=22833678&DFA_PlacementId=249669610&DFA_AdId=446047084&DFA_CreativeId=110252429&DFA_SiteId=3654125&TC_1=2000138&TC_2=22833678&TC_3=249669610&TC_4=110252429&ct=SE&st=&city=11928&dma=0&zp=&bw=4&Placement_ID=249669610" + "&edge=" + edgeSupport + "&html5="+ html5Support +"&nr=" + Math.random();
            if(document.readyState === "complete")
            {
                var sc = document.createElement("script");
                sc.setAttribute("type","text/javascript");
                sc.setAttribute("src",newUrl);
                if (document.currentScript) {
                    var pn = document.currentScript.parentNode;
                    var sbn = document.currentScript.nextSibling;
                    if (sbn) {
                        pn.insertBefore(sc,sbn);
                    } else {
                        pn.appendChild(sc);
                    }
                } else {
                    document.body.appendChild(sc);
                }
            } else {
                document.write('<' + 'script type="text/javascript" src="' + newUrl +'"></' + 'script>');
            }
        }

     function getInternetExplorerVersion_281685() {
         // Returns the version of Internet Explorer or a -1
         // (indicating the use of another browser).

             var rv = -1; // Return value assumes failure.
             if (navigator.appName == 'Microsoft Internet Explorer') {
                 isIEBrowser_281685=true;
                 var ua = navigator.userAgent;
                 var re  = new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})");

                 if (re.exec(ua) != null)
                     rv = parseFloat( RegExp.$1 );
             }

             return rv;
         }

      //returns true if ie version is less than 9, say ie6, ie7, ie8
         // -1 for non IE browsers.
         function isIEBrowserWithVersionLessThan9_281685 () {

             browserVersion_281685 = getInternetExplorerVersion_281685();  //-1 for non IE browsers
             if((browserVersion_281685 != -1) && (browserVersion_281685 < 9)) {
                 return true;

             }
             return false;
         }

    //code to detect Edge Features, courtesy  (http://dl.dropboxusercontent.com/u/13483458/test-edge.html)
    var testEle_281685=document.createElement("div_281685");
    function isSupported_281685(a){

        var d=testEle_281685.style,e;
        for(i=0;i<a.length;i++)
            if(e=a[i],d[e]!==void 0)
                return!0;
        return!1
    }

    function supportsRGBA_281685(){

        testEle_281685.cssText="background-color:rgba(150,255,150,.5)";
        if((""+testEle_281685.style.backgroundColor).indexOf("rgba")==0)
            return!0;
        return!1
    }

    var hasTransform_281685=isSupported_281685([
        "transformProperty",
        "WebkitTransform",
        "MozTransform",
        "OTransform",
        "msTransform"
    ]),

    hasSVG_281685=!!document.createElementNS&&!!document.createElementNS("http://www.w3.org/2000/svg","svg").createSVGRect,
    hasRGBA_281685=supportsRGBA_281685(),
    hasJSON_281685=window.JSON&&window.JSON.parse&&window.JSON.stringify,
    readyToPlay=!1;

    function isIEBrowserVersion9_281685() {
        return (isIEBrowser_281685 && (browserVersion_281685 == 9)) ? true : false;
    }

    function isEdgeSupported_281685() {
        if(isIEBrowserVersion9_281685()) {
            return "y";           //hardcoding IE9 edge support.
        }
        if(hasTransform_281685) {
            if(requiresSVG_281685&&!hasSVG_281685)
                return "f";
            return "y";
        }
        return "f";
    }

    function isCanvasSupported_281685(){
      var elem = document.createElement('canvas');
      return !!(elem.getContext && elem.getContext('2d'));
    }

    function isHTML5FeaturesSupported_281685() {
         return (isCanvasSupported_281685()) ? "y" : "f";
    }

    var requiresSVG_281685=false;
    //edge detection code end

    //Edge is not supported in IE 6,7,8. Hence hardcoding edge as not supported for the same.
   // edgeSupport_281685 = (isIEBrowserWithVersionLessThan9_281685()) ? "f" : isHTMLFeaturesSupported_281685(featureArray_281685);
    edgeSupport_281685 = (isIEBrowserWithVersionLessThan9_281685()) ? "f" : isEdgeSupported_281685();
    html5Support_281685 = isHTML5FeaturesSupported_281685();

    initiateNewRequest_281685(edgeSupport_281685, html5Support_281685);
