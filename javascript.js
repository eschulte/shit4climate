//                                               -*- mode:javascript -*-

// Order defined in `representative-details' in update.lisp.
function full_name(legislator){ return legislator[0] }
function phone(legislator){ return legislator[1] }
function email(legislator){ return legislator[2] }
function contact(legislator){ return legislator[3] }
function denier(legislator){ return legislator[4] }

const local_storage_version = 1;
function getVersioned(key){
  const version = JSON.parse(localStorage.getItem('version'))
  if(version && (version == local_storage_version)){
    return localStorage.getItem(key)
  } else {
    localStorage.removeItem(key)
    return null;
  }
}

function setVersioned(key, value){
  localStorage.setItem('version', local_storage_version)
  return localStorage.setItem(key, value)
}

// From https://stackoverflow.com/questions/12460378/how-to-get-json-from-url-in-javascript
var remote_zip = function(zip, callback){
  var xhr = new XMLHttpRequest()
  url = '/zip-data/'+zip.slice(0,2)+'/'+zip+'.json'
  xhr.open('GET', url, true)
  xhr.responseType = 'json'
  xhr.onload = function() {
    var status = xhr.status
    var by_zip = JSON.parse(getVersioned('by-zip'))
    if (status === 200) {
      console.log('Responded 200 for '+url+' with '+xhr.response)
      by_zip[zip] = xhr.response
    } else {
      console.log('No JSON found for '+zip)
      by_zip[zip] = false
    }
    setVersioned('by-zip', JSON.stringify(by_zip))
    callback(by_zip[zip])
  }
  xhr.send()
}

// Given a ZIP call CALLBACK on it's data from (1) local storage or (2) a remote JSON file.
function zip_data(zip, callback){
  var by_zip = getVersioned('by-zip')
  if(! by_zip){
    by_zip = {}
    setVersioned('by-zip', JSON.stringify(by_zip))
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
    var denier_str = ""
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
    if(denier(rep)){
      denier_str = " <a class=\"denier\" href=\"/deniers/"+denier(rep)+"\">DENIER</a>"
    }
    element.innerHTML += "<span class=\"w3-hide-small\">Contact </span>"+position+" <b>"+full_name(rep)+denier_str+"</b>:"+
      phone_str+
      email_str+
      contact_str
  })
}

function zip_key_press(){
  var zip_element = document.getElementById("postal-code");
  var partial_zip = zip_element.value;

  if(partial_zip.length >= 5){
    localStorage.setItem('zip', partial_zip)
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
  // Nice footnotes for links.
  let footnote_counter = 1
  let footnotes = ['']
  Array.from(body_dom.getElementsByTagName('A')).forEach(e => {
    e.text += ' ['+footnote_counter+']';
    footnotes[footnote_counter] = e.href;
    footnote_counter++;
  })
  let footnote_str = ''
  for(let i=1; i < footnote_counter; i++){
    footnote_str += '['+i+'] '+footnotes[i]+'\n'
  }
  Array.from(body_dom.getElementsByTagName('LI')).forEach(e => {
    e.children[0].innerHTML = '• '+e.children[0].innerHTML+'<br>'
  })
  Array.from(body_dom.getElementsByTagName('UL')).forEach(e => {
    e.innerHTML = '<br><br>'+e.innerHTML
  })

  let body = body_dom.textContent;

  // Strip <br>s from body text.
  body = body.replace(/<br><br>/gi, '\n').replace(/<br>/gi, ' ')

  Array.from(document.getElementsByClassName("email-link")).forEach(link => {
    link.href += "?subject="+encodeURI(subject)+"&body="+encodeURI(
      "Representative "+link.id+",\n"+body+"\nVery Respectfully,\n\n"+footnote_str)
  })
}

function outside_on_load(){
  if(localStorage.getItem('zip')){
    const partial_zip = localStorage.getItem('zip');
    zip_data(partial_zip, data => {
      element = document.getElementById("contact")
      data['sen'].concat(data['rep']).concat(data['state']).
        forEach(rep => {
          var phone_str = ""
          var email_str = ""
          var contact_str = ""
          var denier_str = ""
          if(phone(rep)){
            phone_str = "<a href=\"tel:+1"+phone(rep)+"\" title=\" Call: "+phone(rep)+"\"></a> "
          }
          if(email(rep)){
            email_str = "<a class=\"email-link\" id=\""+full_name(rep)+"\" href=\"mailto:"+email(rep)+"\" title=\" Email: "+email(rep)+"\"></a> "
          }
          if(contact(rep)){
            contact_str = "<a class=\"external\" href=\""+contact(rep)+"\" title=\" Contact form: "+contact(rep)+"\"></a> "
          }
          if(denier(rep)){
            denier_str = " <a class=\"denier\" href=\"/deniers/"+denier(rep)+"\">DENIER</a>"
          }
          element.innerHTML += "<b>"+full_name(rep)+"</b>"+denier_str+" at: "+phone_str+email_str+contact_str+"</br>"
        })
    })
  }
  write_emails()
}

function populate_zip(){
  var zip_element = document.getElementById("postal-code");
  zip_element.value = localStorage.getItem('zip');
  zip_key_press();
}
