Feature: Oneprovider Metadata view
  Various operations on Metadata view

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started

  Scenario: Open metadata panel and check presence of navigation tabs
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    Then user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser sees [Basic, JSON, RDF] navigation tabs in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Edit metadata icon is visible if file has empty basic metadata entry
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser should not see metadata icon for file named "file1"
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    Then user of browser sees metadata icon for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Edit metadata icon is visible if directory has empty basic metadata entry
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser should not see metadata icon for directory named "dir1"
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    Then user of browser sees metadata icon for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Invalid basic metadata entry for file should be colored red
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    Then user of browser sees that edited attribute key in metadata panel opened for file named "file1" is highlighted as invalid

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Invalid basic metadata entry for directory should be colored red
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    Then user of browser sees that edited attribute key in metadata panel opened for directory named "dir1" is highlighted as invalid

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Entered invalid metadata for file will not be saved
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    # try saving empty forms
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser refreshes site

    # try saving metadata with record key being filled only
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for file named "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"

    Then user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Entered invalid metadata for directory will not be saved
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    # try saving empty forms
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser refreshes site

    # try saving metadata with record key being filled only
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    Then user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Add metadata to file (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for file named "file1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for file named "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Add metadata to directory (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for directory named "dir1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Add metadata to file (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for file named "file1"
    And user of browser types "val" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Add metadata to directory (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for directory named "dir1"
    And user of browser types "val" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Delete single basic metadata entry for file
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for file named "file1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for file named "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on delete basic metadata entry icon for basic metadata entry with attribute named "attr" in metadata panel opened for file named "file1"
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Delete single basic metadata entry for directory
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for directory named "dir1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on delete basic metadata entry icon for basic metadata entry with attribute named "attr" in metadata panel opened for directory named "dir1"
    Then user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: User should not see any metadata for file after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for file named "file1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for file named "file1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for file named "file1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    Then user of browser should not see any basic metadata entry in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: User should not see any metadata for directory after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for directory named "dir1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser should see basic metadata entry with attribute named "attr" and value "val" in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    Then user of browser should not see any basic metadata entry in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: User starts adding metadata to file but discards changes
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for file named "file1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for file named "file1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for file named "file1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for file named "file1"
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    Then user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for file named "file1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: User starts adding metadata to directory but discards changes
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on input box with placeholder equal to "Attribute" in metadata panel opened for directory named "dir1"
    And user of browser types "attr" on keyboard
    And user of browser clicks on input box with placeholder equal to "Value" in metadata panel opened for directory named "dir1"
    And user of browser types "val" on keyboard
    And user of browser clicks on add basic metadata entry icon in metadata panel opened for directory named "dir1"
    And user of browser clicks on "Discard changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    Then user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser should not see basic metadata entry with attribute named "attr" in metadata panel opened for directory named "dir1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Add valid metadata to file in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that textarea placed in metadata panel opened for file named "file1" contains ""id": 1"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Add valid metadata to directory in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that textarea placed in metadata panel opened for directory named "dir1" contains ""id": 1"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Delete file metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that textarea placed in metadata panel opened for file named "file1" contains ""id": 1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for file named "file1"
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that content of textarea placed in metadata panel opened for file named "file1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Delete directory metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that textarea placed in metadata panel opened for directory named "dir1" contains ""id": 1"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that content of textarea placed in metadata panel opened for directory named "dir1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Discard changes while entering metadata for file in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Discard changes" button in metadata panel opened for file named "file1"
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    Then user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that content of textarea placed in metadata panel opened for file named "file1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Discard changes while entering metadata for directory in JSON format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Discard changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared

    Then user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that content of textarea placed in metadata panel opened for directory named "dir1" is equal to: "{}"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Add valid metadata to file in XML format
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that textarea placed in metadata panel opened for file named "file1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Add valid metadata to directory in RDF format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that textarea placed in metadata panel opened for directory named "dir1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Delete file metadata in XML format
    When user of browser uses spaces select to change data space to "space1"

    # create file1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for file named "file1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared
    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that textarea placed in metadata panel opened for file named "file1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for file named "file1"
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that content of textarea placed in metadata panel opened for file named "file1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Delete directory metadata in XML format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared
    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that textarea placed in metadata panel opened for directory named "dir1" contains "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>"
    And user of browser clicks on "Remove metadata" button in metadata panel opened for directory named "dir1"
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that content of textarea placed in metadata panel opened for directory named "dir1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list


  Scenario: Discard changes while entering metadata for file in XML format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    And user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser clicks on textarea placed in metadata panel opened for file named "file1"
    And user of browser clears textarea placed in metadata panel opened for file named "file1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Discard changes" button in metadata panel opened for file named "file1"
    And user of browser sees that metadata panel for file named "file1" in files list has disappeared

    Then user of browser selects "file1" from files list
    And user of browser clicks on metadata icon in file row for file named "file1" in file browser
    And user of browser sees that metadata panel for file named "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for file named "file1"
    And user of browser sees that content of textarea placed in metadata panel opened for file named "file1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: Discard changes while entering metadata for directory in XML format
    When user of browser uses spaces select to change data space to "space1"

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has appeared on files list

    And user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser clicks on textarea placed in metadata panel opened for directory named "dir1"
    And user of browser clears textarea placed in metadata panel opened for directory named "dir1"
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Discard changes" button in metadata panel opened for directory named "dir1"
    And user of browser sees that metadata panel for directory named "dir1" in files list has disappeared

    Then user of browser selects "dir1" from files list
    And user of browser clicks on metadata icon in file row for directory named "dir1" in file browser
    And user of browser sees that metadata panel for directory named "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata panel opened for directory named "dir1"
    And user of browser sees that content of textarea placed in metadata panel opened for directory named "dir1" is equal to: ""

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir1" has disappeared from files list
