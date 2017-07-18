Feature: Basic data tab operations on directory metadata in file browser


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: user1
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000
            storage:
                defaults:
                    provider: p1
                directory tree:
                    - dir1

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started


  Scenario: Edit metadata icon is visible if directory has empty basic metadata entry
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser does not see metadata icon for "dir1" in file browser
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser clicks on "Save all changes" button in metadata panel opened for "dir1"
    And user of browser sees an info notify with text matching to: .*[Mm]etadata.*saved.*successfully.*
    And user of browser refreshes site
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees metadata icon for "dir1" in file browser


  Scenario: Invalid basic metadata entry for directory should be colored red
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks on metadata tool icon in file row for "dir1" in file browser
    And user of browser sees that metadata panel for "dir1" in files list has appeared
    And user of browser types "attr" to attribute input box of new metadata basic entry in metadata panel opened for "dir1"
    Then user of browser sees that edited attribute key in metadata panel opened for "dir1" is highlighted as invalid


  Scenario: Entered invalid metadata for directory will not be saved
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Add metadata to directory (clicks both add icon and "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Add metadata to directory (clicks only "Save all changes" button)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Delete single basic metadata entry for directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: User should not see any metadata for directory after clicking "Remove metadata" button
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: User starts adding metadata to directory but discards changes
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Add valid metadata to directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Delete directory metadata in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Discard changes while entering metadata for directory in JSON format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Add valid metadata to directory in RDF format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Delete directory metadata in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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


  Scenario: Discard changes while entering metadata for directory in XML format
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

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
