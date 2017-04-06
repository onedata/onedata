Feature: Oneprovider functionality using multiple providers and cdmi service

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser logged as user1
    And user of browser created and recorded access token for later use with CDMI API
    And user of browser records providers hostname using copy hostname button in every provider popup
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User uploads file on one provider, sees it's distribution, writes to it using cdmi on other provider and sees it's distribution
    When user of browser uses spaces select to change data space to "space4"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space4
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    And user of browser sees that chunk bar for provider named "p1" is entirely filled
    And user of browser sees that chunk bar for provider named "p2" is entirely empty
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser writes "ABCD" to "/space4/20B-0.txt" starting at offset 20 in "p2" provider using cdmi api
    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    Then user of browser sees (0, 20) chunk(s) for provider named "p1" in chunk bar
    And user of browser sees (20, 24) chunk(s) for provider named "p2" in chunk bar

    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User uploads file, sees it's size, writes to it using cdmi and sees that size has grown
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser sees that item named "20B-0.txt" is of 20 B size in file browser
    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    And user of browser sees that chunk bar for provider named "p1" is of 20 B size
    And user of browser sees that chunk bar for provider named "p1" is entirely filled
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser writes "ABCD" to "/space1/20B-0.txt" starting at offset 20 in "p1" provider using cdmi api
    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser sees that item named "20B-0.txt" is of 24 B size in file browser
    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    Then user of browser sees that chunk bar for provider named "p1" is of 24 B size
    And user of browser sees that chunk bar for provider named "p1" is entirely filled

    # TODO rm after integrating with swagger
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User uploads file on one provider, sees it's distribution, reads half of file on other provider using cdmi and again sees it's distribution
    When user of browser uses spaces select to change data space to "space4"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space4
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    And user of browser sees that chunk bar for provider named "p1" is entirely filled
    And user of browser sees that chunk bar for provider named "p2" is entirely empty
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser reads from "/space4/20B-0.txt" in range 10 to 20 in "p2" provider using cdmi api
    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    And user of browser sees that chunk bar for provider named "p1" is entirely filled
    Then user of browser sees (10, 20) chunk(s) for provider named "p2" in chunk bar

    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User uploads file, sees it's distribution, writes to it beyond the end of file using cdmi and sees it's distribution again
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    And user of browser sees that chunk bar for provider named "p1" is entirely filled
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser writes "ABCD" to "/space1/20B-0.txt" starting at offset 40 in "p1" provider using cdmi api
    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    Then user of browser sees [(0, 20), (40, 44)] chunk(s) for provider named "p1" in chunk bar

    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel


  Scenario: User uploads file, appends some text to it, downlaods it and sees it's content
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser writes "ABCD" to "/space1/20B-0.txt" starting at offset 20 in "p1" provider using cdmi api
    And user of browser is idle for 30 seconds
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser double clicks on item named "20B-0.txt" in file browser
    Then user of browser sees that content of downloaded file "20B-0.txt" is equal to: "00000000000000000000ABCD"

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser clicks on the "providers" tab in main menu sidebar
    And user of browser expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
