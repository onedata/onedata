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

  Scenario: Open metadata submenu and check presence of navigation tabs
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    Then user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser sees [Basic, JSON, RDF] navigation tabs in opened metadata submenu
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Edit metadata icon is visible if file has empty metadata record
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser should not see metadata icon for file "file1"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    Then user of browser sees metadata icon for file "file1"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Edit metadata icon is visible if directory has empty metadata record
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser should not see metadata icon for directory "dir1"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    Then user of browser sees metadata icon for directory "dir1"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Invalid metadata record for file should be colored red and should not be saved
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    Then user of browser sees that entered metadata record with attribute "attr" is red
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Invalid metadata record for directory should be colored red and should not be saved
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    Then user of browser sees that entered metadata record with attribute "attr" is red
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: When entered metadata for file is invalid "Save all changes" button should be disabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should not see new metadata record with attribute "attr"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: When entered metadata for directory is invalid "Save all changes" button should be disabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser should not see metadata icon for directory "dir1"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser should not see new metadata record with attribute "attr"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

    #TODO only one metadata submenu can be active at time
  Scenario: Add metadata to file (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Add metadata to directory (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Add metadata to file (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Add metadata to directory (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Delete single metadata record for file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on delete metadata record icon for metadata record with attribute "attr"
    Then user of browser should not see new metadata record with attribute "attr"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Delete single metadata record for directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on delete metadata record icon for metadata record with attribute "attr"
    Then user of browser should not see new metadata record with attribute "attr"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: User should not see any metadata for file after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser should not see any metadata record for "file1"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: User should not see any metadata for directory after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser should see new metadata record with attribute "attr" and value "val"
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser should not see any metadata record for "dir1"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: User starts adding metadata to file but discards changes
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should not see new metadata record with attribute "attr"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: User starts adding metadata to directory but discards changes
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Value" input box for new metadata record with attribute "attr"
    And user of browser types "val" on keyboard
    And user of browser clicks on add icon in metadata submenu
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser should not see new metadata record with attribute "attr"
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Add valid metadata to file in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser sees that textarea in "JSON" navigation tab has got ""id": 1" metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Add valid metadata to directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser sees that textarea in "JSON" navigation tab has got ""id": 1" metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Delete file metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser sees that textarea in "JSON" navigation tab has got ""id": 1" metadata record
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser should see that textarea in "JSON" navigation tab hasn't got any metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Delete directory metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser sees that textarea in "JSON" navigation tab has got ""id": 1" metadata record
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser should see that textarea in "JSON" navigation tab hasn't got any metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Discard changes while entering metadata for file in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser should see that textarea in "JSON" navigation tab hasn't got any metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Discard changes while entering metadata for directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser clicks on textarea in "JSON" navigation tab
    And user of browser clears textarea in "JSON" navigation tab
    And user of browser types "{"id": 1}" on keyboard
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "JSON" navigation tab in metadata submenu
    And user of browser should see that textarea in "JSON" navigation tab hasn't got any metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: When entered metadata in JSON format for file is invalid "Save all changes" button should be disabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser should not see metadata icon for file "file1"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "Attribute" input box
    And user of browser types "attr" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser should not see new metadata record with attribute "attr"
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Add valid metadata to file in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser sees that textarea in "XML" navigation tab has got "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Add valid metadata to directory in RDF format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser sees that textarea in "XML" navigation tab has got "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Delete file metadata in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser sees that textarea in "XML" navigation tab has got "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" metadata record
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*file1.*
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser should see that textarea in "XML" navigation tab hasn't got any metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Delete directory metadata in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Save all changes" button in metadata submenu
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser sees that textarea in "XML" navigation tab has got "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" metadata record
    And user of browser clicks on "Remove metadata" button in metadata submenu
    Then user of browser sees an info notify with text matching to: .*[Dd]eleted.*metadata.*dir1.*
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser should see that textarea in "XML" navigation tab hasn't got any metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list

  Scenario: Discard changes while entering metadata for file in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees that file named "file1" has appeared in file list
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects file1 from files list
    And user of browser sees that metadata submenu for file "file1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser should see that textarea in "XML" navigation tab hasn't got any metadata record
    And user of browser selects file1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that file named "file1" has disappeared from file list

  Scenario: Discard changes while entering metadata for directory in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "dir1" has appeared in file list
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser clicks on textarea in "XML" navigation tab
    And user of browser clears textarea in "XML" navigation tab
    And user of browser types "<rdf:XML xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"></rdf:XML>" on keyboard
    And user of browser clicks on "Discard changes" button in metadata submenu
    Then user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Edit metadata"
    And user of browser deselects dir1 from files list
    And user of browser sees that metadata submenu for directory "dir1" in files list has appeared
    And user of browser clicks on "RDF" navigation tab in metadata submenu
    And user of browser should see that textarea in "XML" navigation tab hasn't got any metadata record
    And user of browser selects dir1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that directory named "dir1" has disappeared from file list
