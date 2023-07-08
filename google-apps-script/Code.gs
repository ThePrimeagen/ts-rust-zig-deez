const input =`
  let five = 5;
  let ten = 10;
  let add = fn(x, y) {
      x + y;
  };
  let result = add(five, ten);
  !-/*5;
  5 < 10 > 5;
  if (5 < 10) {
      return true;
  } else {
      return false;
  }

  10 == 10;
  10 != 9;
`;


function test() {
  const tokenizer = new Tokenizer(input);

  while (true) {
    const token = tokenizer.getNextToken()
    console.log(token);
    if (token.type === "EOF") {
      break;
    }
  }
  
}

// Manage script configuration here
const g = {
  subjectFilter: 'pls lexer deez',
  labelName: 'theNAMEisThePrimeagen',
  maxThreads: 100,
  startingThread: 0,
  replyCount: 0,
};

/**
 * Check for new emails and reply if needed.
 */
function checkAndReply() {
  // Prepare the script
  init_();
  while (true) {
    // Search for new threads
    const threads = GmailApp.search(g.query, g.startingThread, g.maxThreads);
    g.startingThread += g.maxThreads;
    threads.forEach((thread) => {

      var messages = GmailApp.getMessagesForThread(thread);

      var allText = "";

      for (var j=0; j<messages.length; j++) {
        if (messages[j].isUnread()) {
          var msg = messages[j].getPlainBody();      

          const tokenizer = new Tokenizer(msg);

          while (true) {
            const token = tokenizer.getNextToken()
            allText += (token.type + " ----> " + token.literal + "\n")
            if (token.type === "EOF") {
              break;
            }
          }
        }
      }

      // Reply to the thread
      thread.reply(allText);
      // Label the thread as auto-replied
      g.label.addToThread(thread);
      // Count the replies
      g.replyCount++;
    });
    // Stop the loop if there are no more
    // new threads to process
    if (threads.length < g.maxThreads) {
      break;
    }
  }
  // Log the status
  const text =
    g.replyCount == 0
      ? 'No new emails found.'
      : g.replyCount == 1
      ? 'Replied to one new email.'
      : `Replied to ${g.replyCount} new emails.`;
  console.log(text);
}

/**
 * Initialize the automation
 */
function init_() {
  // Prepare the Gmail label
  const labels = GmailApp.getUserLabels().map((l) => l.getName());
  if (!labels.includes(g.labelName)) {
    GmailApp.createLabel(g.labelName);
  }
  g.label = GmailApp.getUserLabelByName(g.labelName);
  // Prepare the search filter for emails containing
  // specific keywords in the subject line
  // and emails without the responded-to label.
  const queryFilter = {
    subject: g.subjectFilter,
    '-label': g.labelName,
  };
  g.query = Object.entries(queryFilter)
    .map((e) => e.join(':'))
    .join(' ');
  // Prepare the script trigger to run this script automatically.
  const triggers = ScriptApp.getProjectTriggers();
  if (triggers.length == 0) {
    ScriptApp.newTrigger('checkAndReply').timeBased().everyMinutes(1).create();
  }
}

// Made with ❤️ by aliyss (GitHub)
// Inspired by ... the NAME is ThePrimeagen (go subscribe)
// Found the Gmail AutoResponder from benronkink (GitHub)
