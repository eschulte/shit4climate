// {% capture js %}
// {% include javascript %}
// {% endcapture %}
// {{ js | jsmin | safe }}
//                                               -*- mode:javascript -*-

// Order defined in `representative-details' in update.lisp.
function full_name(legislator){ return legislator[0] }
function phone(legislator){ return legislator[1] }
function email(legislator){ return legislator[2] }
function contact(legislator){ return legislator[3] }

// From https://stackoverflow.com/questions/12460378/how-to-get-json-from-url-in-javascript
var remote_zip = function(zip, callback){
  var xhr = new XMLHttpRequest()
  url = '/zip-data/'+zip+'.json'
  xhr.open('GET', url, true)
  xhr.responseType = 'json'
  xhr.onload = function() {
    var status = xhr.status
    var by_zip = JSON.parse(localStorage.getItem('by-zip'))
    if (status === 200) {
      console.log('Responded 200 for '+url+' with '+xhr.response)
      by_zip[zip] = xhr.response
    } else {
      console.log('No JSON found for '+zip)
      by_zip[zip] = false
    }
    localStorage.setItem('by-zip', JSON.stringify(by_zip))
    callback(by_zip[zip])
  }
  xhr.send()
}

// Given a ZIP call CALLBACK on it's data from (1) local storage or (2) a remote JSON file.
function zip_data(zip, callback){
  var by_zip = localStorage.getItem('by-zip')
  if(! by_zip){
    by_zip = {}
    localStorage.setItem('by-zip', JSON.stringify(by_zip))
  } else {
    by_zip = JSON.parse(by_zip)
  }
  if(by_zip.hasOwnProperty(zip)) {
    callback(by_zip[zip])
  } else {
    remote_zip(zip, callback)
  }
}

function populate_match(element, position, representatives){
  element.innerHTML = ""
  representatives.forEach(rep => {
    var phone_str = ""
    var email_str = ""
    var contact_str = ""
    if(phone(rep)){
      phone_str = "<br/><a href=\"tel:+1"+phone(rep)+"\">"+phone(rep)+"</a>"
    }
    if(email(rep)){
      email_str = "<br/><a href=\"mailto:"+email(rep)+"\">"+email(rep)+"</a>"
    }
    if(contact(rep)){
      contact_str = "<br/><a class=\"external\" href=\""+contact(rep)+"\">"+contact(rep)+"</a>"
    } else {
      // Only line break if no contact (contact is display:block; and creates vertical space).
      contact_str = "<br/>"
    }
    element.innerHTML += "Contact "+position+" <b>"+full_name(rep)+"</b>:"+
      phone_str+
      email_str+
      contact_str
  })
}

function zip_key_press(){
  var zip_element = document.getElementById("postal-code");
  var partial_zip = zip_element.value;

  if(partial_zip.length >= 5){
    zip_data(partial_zip, data => {
      if(data){
        populate_match(document.getElementById("call-senator-by-zip"),
                       "Senator", data['sen'])
        populate_match(document.getElementById("call-representative-by-zip"),
                       "Representative", data['rep'])
        populate_match(document.getElementById("call-state-representative-by-zip"),
                       "State Representative", data['state'])

      } else {
        document.getElementById("call-representative-by-zip").innerHTML =
          "No representatives found for <b>"+partial_zip+"</b>."
        document.getElementById("call-senator-by-zip").innerHTML = ""
        document.getElementById("call-state-representative-by-zip").innerHTML = ""
      }
    })
  } else {
    // Storage.
    localStorage.removeItem('zip');
    document.getElementById("call-representative-by-zip").innerHTML = "";
    document.getElementById("call-senator-by-zip").innerHTML = "";
    document.getElementById("call-state-representative-by-zip").innerHTML = "";
  }
}

function write_emails(){
  let subject = document.getElementById("title").textContent;
  let body_dom = document.getElementById("content").cloneNode(true);
  // Drop blockquotes from body text.
  Array.from(body_dom.children).forEach(e => {
    if(e.tagName == "BLOCKQUOTE"){
      body_dom.removeChild(e)
    }
  })
  Array.from(body_dom.getElementsByTagName("A")).forEach(e => {
    e.text += " ("+e.href+")"
  })
  let body = body_dom.textContent;

  Array.from(document.getElementsByClassName("email-link")).forEach(link => {
    link.href += "?subject="+encodeURI(subject)+"&body="+encodeURI(
      "Representative "+link.id+",\n"+body+"\n\nVery Respectfully,\n")
  })
}

function outside_on_load(){
  if(localStorage.getItem('zip')){
    const partial_zip = localStorage.getItem('zip');
    var match = zipStateDistrict[partial_zip];
    var stateDistrict = match[0]+match[1];
    console.log("MATCH:"+stateDistrict);
    element = document.getElementById("contact")

    stateSenator[match[0]].
      concat(stateDistrictRepresentative[stateDistrict]).
      concat(stateDistrictStateRepresentative[stateDistrict]).
      forEach(rep => {
        var phone_str = ""
        var email_str = ""
        var contact_str = ""
        if(phone(rep)){
          phone_str = "<a href=\"tel:+1"+phone(rep)+"\" title=\" Call: "+phone(rep)+"\"></a> "
        }
        if(email(rep)){
          email_str = "<a class=\"email-link\" id=\""+full_name(rep)+"\" href=\"mailto:"+email(rep)+"\" title=\" Email: "+email(rep)+"\"></a> "
        }
        if(contact(rep)){
          contact_str = "<a class=\"external\" href=\""+contact(rep)+"\" title=\" Contact form: "+contact(rep)+"\"></a> "
        }
        element.innerHTML += "<b>"+full_name(rep)+"</b> at: "+phone_str+email_str+contact_str+"</br>"
      })
  }
  write_emails()
}

function populate_zip(){
  var zip_element = document.getElementById("postal-code");
  zip_element.value = localStorage.getItem('zip');
  zip_key_press();
}
