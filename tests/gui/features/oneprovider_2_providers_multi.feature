Feature: Oneprovider functionality using multiple providers and multiple browsers

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened Onezone URL
    And users of [browser1, browser2] clicked on the "plgrid" login button
    And users of [browser1, browser2] logged as [user1, user3]
    And user of browser2 created and recorded access token for later use with CDMI API
    And user of browser2 records providers hostname using copy hostname button in every provider popup
    And users of [browser1, browser2] expanded the "go to your files" Onezone sidebar panel
    And users of [browser1, browser2] clicked on the ["p1", "p2"] provider in Onezone providers sidebar panel
    And users of [browser1, browser2] clicked on the "Go to your files" button in provider popup
    And users of [browser1, browser2] seen that Oneprovider session has started


#  Scenario: User uploads file on one provider, sees it's distribution, downloads on other provider and again sees it's distribution
#    When user of browser1 uses spaces select to change data space to "space4"
#    And user of browser1 sees file browser in data tab in Oneprovider page
#    And user of browser1 sees that current working directory displayed in breadcrumbs is space4
#    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
#    And user of browser1 sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
#    And user of browser1 sees that item named "20B-0.txt" has appeared in file browser
#
#    # check file distribution
#    And user of browser1 clicks once on file named "20B-0.txt" of files list
#    And user of browser1 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser1 sees that "File distribution" modal has appeared
#    Then user of browser1 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser1 clicks "Close" confirmation button in displayed modal
#    And user of browser1 sees that the modal has disappeared
#
#    And user of browser2 is idle for 90 seconds
#    And user of browser2 refreshes site
#    And user of browser2 uses spaces select to change data space to "space4"
#    And user of browser2 sees file browser in data tab in Oneprovider page
#    And user of browser2 sees that current working directory displayed in breadcrumbs is space4
#    And user of browser2 clicks once on file named "20B-0.txt" of files list
#    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser2 sees that "File distribution" modal has appeared
#    And user of browser2 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser2 sees that chunk bar for provider named "p2" is entirely empty
#    And user of browser2 clicks "Close" confirmation button in displayed modal
#    And user of browser2 sees that the modal has disappeared
#    And user of browser2 double clicks on item named "20B-0.txt" in file browser
#    And user of browser2 sees that content of downloaded file "20B-0.txt" is equal to: "00000000000000000000"
#
#    And user of browser2 clicks once on file named "20B-0.txt" of files list
#    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser2 sees that "File distribution" modal has appeared
#    And user of browser2 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser2 sees that chunk bar for provider named "p2" is entirely filled
#    And user of browser2 clicks "Close" confirmation button in displayed modal
#    And user of browser2 sees that the modal has disappeared
#
#    And user of browser1 is idle for 90 seconds
#    And user of browser1 refreshes site
#    And user of browser1 sees file browser in data tab in Oneprovider page
#    And user of browser1 clicks once on file named "20B-0.txt" of files list
#    And user of browser1 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser1 sees that "File distribution" modal has appeared
#    And user of browser1 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser1 sees that chunk bar for provider named "p2" is entirely filled
#    And user of browser1 clicks "Close" confirmation button in displayed modal
#    And user of browser1 sees that the modal has disappeared
#
#    # TODO rm after integrating with swagger
#    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser1 sees that "Remove files" modal has appeared
#    And user of browser1 clicks "Yes" confirmation button in displayed modal
#    And user of browser1 sees an info notify with text matching to: .*removed.*
#    And user of browser1 sees that the modal has disappeared
#    And user of browser1 sees that item named "20B-0.txt" has disappeared from files browser
#    And user of browser2 clicks on the "providers" tab in main menu sidebar
#    And user of browser2 expands the "ACCESS TOKENS" Onezone sidebar panel
#    And user of browser2 clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
#    And user of browser2 sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
#
#
#  Scenario: User uploads file on one provider, sees it's distribution, writes to it using cdmi on other provider and sees it's distribution
#    When user of browser1 uses spaces select to change data space to "space4"
#    And user of browser1 sees file browser in data tab in Oneprovider page
#    And user of browser1 sees that current working directory displayed in breadcrumbs is space4
#    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
#    And user of browser1 sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
#    And user of browser1 sees that item named "20B-0.txt" has appeared in file browser
#
#    # check file distribution on p1
#    And user of browser1 clicks once on file named "20B-0.txt" of files list
#    And user of browser1 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser1 sees that "File distribution" modal has appeared
#    Then user of browser1 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser1 clicks "Close" confirmation button in displayed modal
#    And user of browser1 sees that the modal has disappeared
#
#    # check file distribution on p2 (otherwise file will not be created on p2)
#    And user of browser2 is idle for 90 seconds
#    And user of browser2 refreshes site
#    And user of browser2 uses spaces select to change data space to "space4"
#    And user of browser2 sees file browser in data tab in Oneprovider page
#    And user of browser2 sees that current working directory displayed in breadcrumbs is space4
#    And user of browser2 clicks once on file named "20B-0.txt" of files list
#    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser2 sees that "File distribution" modal has appeared
#    And user of browser2 sees that chunk bar for provider named "p1" is entirely filled
#    And user of browser2 sees that chunk bar for provider named "p2" is entirely empty
#    And user of browser2 clicks "Close" confirmation button in displayed modal
#    And user of browser2 sees that the modal has disappeared
#
#    And user of browser2 writes "ABCD" to "/space4/20B-0.txt" starting at offset 20 in "p2" provider using cdmi api
#    And user of browser2 is idle for 90 seconds
#    And user of browser2 refreshes site
#    And user of browser2 sees file browser in data tab in Oneprovider page
#
#    And user of browser2 clicks once on file named "20B-0.txt" of files list
#    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser2 sees that "File distribution" modal has appeared
#    Then user of browser2 sees (0, 20) chunk(s) for provider named "p1" in chunk bar
#    And user of browser2 sees (20, 24) chunk(s) for provider named "p2" in chunk bar
#
#    And user of browser2 clicks "Close" confirmation button in displayed modal
#    And user of browser2 sees that the modal has disappeared
#
#    # TODO rm after integrating with swagger
#    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser1 sees that "Remove files" modal has appeared
#    And user of browser1 clicks "Yes" confirmation button in displayed modal
#    And user of browser1 sees an info notify with text matching to: .*removed.*
#    And user of browser1 sees that the modal has disappeared
#    And user of browser1 sees that item named "20B-0.txt" has disappeared from files browser
#    And user of browser2 clicks on the "providers" tab in main menu sidebar
#    And user of browser2 expands the "ACCESS TOKENS" Onezone sidebar panel
#    And user of browser2 clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
#    And user of browser2 sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
#
#
  Scenario: User uploads file on one provider, sees it's distribution, reads half of file on other provider using cdmi and again sees it's distribution
    When user of browser1 uses spaces select to change data space to "space4"
    And user of browser1 sees file browser in data tab in Oneprovider page
    And user of browser1 sees that current working directory displayed in breadcrumbs is space4
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser1 sees that item named "20B-0.txt" has appeared in file browser

    # check file distribution on p1
    And user of browser1 clicks once on file named "20B-0.txt" of files list
    And user of browser1 clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser1 sees that "File distribution" modal has appeared
    Then user of browser1 sees that chunk bar for provider named "p1" is entirely filled
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    # check file distribution on p2 (otherwise file will not be created on p2)
    And user of browser2 is idle for 90 seconds
    And user of browser2 refreshes site
    And user of browser2 uses spaces select to change data space to "space4"
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 sees that current working directory displayed in breadcrumbs is space4
    And user of browser2 clicks once on file named "20B-0.txt" of files list
    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser2 sees that "File distribution" modal has appeared
    And user of browser2 sees that chunk bar for provider named "p1" is entirely filled
    And user of browser2 sees that chunk bar for provider named "p2" is entirely empty
    And user of browser2 clicks "Close" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared

    And user of browser2 reads from "/space4/20B-0.txt" in range 10 to 20 in "p2" provider using cdmi api
    And user of browser2 is idle for 90 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 clicks once on file named "20B-0.txt" of files list
    And user of browser2 clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser2 sees that "File distribution" modal has appeared
    And user of browser2 sees that chunk bar for provider named "p1" is entirely filled
    Then user of browser2 sees (10, 20) chunk(s) for provider named "p2" in chunk bar

    And user of browser2 clicks "Close" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared

    # TODO rm after integrating with swagger
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that item named "20B-0.txt" has disappeared from files browser
    And user of browser2 clicks on the "providers" tab in main menu sidebar
    And user of browser2 expands the "ACCESS TOKENS" Onezone sidebar panel
    And user of browser2 clicks on remove icon for 1st item on tokens list in expanded "ACCESS TOKENS" Onezone panel
    And user of browser2 sees exactly 0 item(s) on tokens list in expanded "ACCESS TOKENS" Onezone panel
