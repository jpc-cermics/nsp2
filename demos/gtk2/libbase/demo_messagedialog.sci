
// MessageDialog 
//----------------------

function messagedialog(t,mess) 
  dialog = gtkmessagedialog_new (flags= GTK.DIALOG_MODAL,
     type= t,
     buttons= GTK.BUTTONS_OK,
     message = mess );
  dialog.run[];
  dialog.destroy[];
endfunction 

function demo_messagedialog() 
  mess_type = [GTK.MESSAGE_INFO;
	       GTK.MESSAGE_WARNING;
	       GTK.MESSAGE_QUESTION;
	       GTK.MESSAGE_ERROR]
  for t = mess_type'
    message(t,"Message" )
  end

  mess = ['kkjhjkhkjhkjhkjhkjhkjh';
	  'gfhg fhfhgf fhgf hg fhgf fhgf hfhg hgf hgfh hgf fhg hgf '
	  'gfhg fhfhgf fhgf hg fhgf fhgf hfhg hgf hgfh hgf fhg hgf '
	  'gfhg fhfhgf fhgf hg fhgf fhgf hfhg hgf hgfh hgf fhg hgf ']
	
  s=catenate(mess,sep='\n'); // XXX Pb de display 
  messagedialog(GTK.MESSAGE_ERROR,s);
endfunction














