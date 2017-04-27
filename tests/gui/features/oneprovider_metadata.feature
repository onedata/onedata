Feature: Oneprovider Metadata view
  Various operations on Metadata view

  Background:
    Given there is user named "user1" in the system
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered credentials of user1 in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "Example Provider" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: Open metadata panel and check presence of navigation tabs
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser sees [Basic, JSON, RDF] navigation tabs in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Edit metadata icon is visible if file has empty basic metadata entry
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser does not see metadata icon for "file1" in file browser
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees metadata icon for "file1" in file browser

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Edit metadata icon is visible if directory has empty basic metadata entry
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser does not see metadata icon for "dir1" in file browser
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees metadata icon for "dir1" in file browser

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Invalid basic metadata entry for file should be colored red
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    Then user of browser sees that edited attribute key in metadata panel opened for "file1" is highlighted as invalid

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Invalid basic metadata entry for directory should be colored red
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    Then user of browser sees that edited attribute key in metadata panel opened for "dir1" is highlighted as invalid

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Entered invalid metadata for file will not be saved
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # try saving empty forms
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    # try saving metadata with record key being filled only
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "file1"
    And user of browser sees that "Save all changes" button in metadata panel opened for "file1" is disabled
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Entered invalid metadata for directory will not be saved
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # try saving empty forms
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page

    # try saving metadata with record key being filled only
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "dir1"
    And user of browser sees that "Save all changes" button in metadata panel opened for "dir1" is disabled
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Add metadata to file (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Add metadata to directory (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    Then user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Add metadata to file (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Add metadata to directory (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    Then user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Delete single basic metadata entry for file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on delete basic metadata entry icon for basic metadata entry with attribute named "attr" in metadata panel opened for "file1"
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Delete single basic metadata entry for directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on delete basic metadata entry icon for basic metadata entry with attribute named "attr" in metadata panel opened for "dir1"
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User should not see any metadata for file after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "file1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for "file1" in files list has disappeared
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User should not see any metadata for directory after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*

    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for "dir1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for "dir1" in files list has disappeared
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User starts adding metadata to file but discards changes
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "file1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "file1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "file1"
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User starts adding metadata to directory but discards changes
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser types "val" to value input box of new metadata basic entry in metadata panel opened for "dir1"
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for "dir1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "dir1"
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Add valid metadata to file in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    Then user of browser sees that JSON textarea placed in metadata panel opened for "file1" contains "{"id": 1}"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Add valid metadata to directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that JSON textarea placed in metadata panel opened for "dir1" contains "{"id": 1}"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Delete file metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    And user of browser sees that JSON textarea placed in metadata panel opened for "file1" contains "{"id": 1}"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    Then user of browser sees that content of JSON textarea placed in metadata panel opened for "file1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Delete directory metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    And user of browser sees that JSON textarea placed in metadata panel opened for "dir1" contains "{"id": 1}"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that content of JSON textarea placed in metadata panel opened for "dir1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Discard changes while entering metadata for file in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "file1"
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "file1"
    Then user of browser sees that content of JSON textarea placed in metadata panel opened for "file1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Discard changes while entering metadata for directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared

    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    And user of browser types "{"id": 1}" to JSON textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "dir1"
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on JSON navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that content of JSON textarea placed in metadata panel opened for "dir1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Add valid metadata to file in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    Then user of browser sees that RDF textarea placed in metadata panel opened for "file1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Add valid metadata to directory in RDF format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that RDF textarea placed in metadata panel opened for "dir1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Delete file metadata in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared

    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    And user of browser sees that RDF textarea placed in metadata panel opened for "file1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "file1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    Then user of browser sees that content of RDF textarea placed in metadata panel opened for "file1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Delete directory metadata in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared

    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    And user of browser sees that RDF textarea placed in metadata panel opened for "dir1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that content of RDF textarea placed in metadata panel opened for "dir1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: Discard changes while entering metadata for file in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared

    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "file1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "file1"
    And user of browser sees that metadata panel for "file1" in files list has disappeared

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "file1" in file browser
    And user of browser sees that metadata panel for "file1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "file1"
    Then user of browser sees that content of RDF textarea placed in metadata panel opened for "file1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: Discard changes while entering metadata for directory in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared

    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" to RDF textarea placed in metadata panel opened for "dir1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for "dir1"
    And user of browser sees that metadata panel for "dir1" in files list has disappeared

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on RDF navigation tab in metadata panel opened for "dir1"
    Then user of browser sees that content of RDF textarea placed in metadata panel opened for "dir1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser
