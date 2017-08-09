Feature: Extended_attributes

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token


  Scenario Outline: Check extended attribute exists
    When u1 creates regular files [s1/file1]
    Then u1 sees file1 in s1
    Then u1 sets on s1/file1 extended attribute <name> with value <value>
    And u1 checks that s1/file1 has extended attribute <name>
    Then u1 removes extended attribute <name> from s1/file1
    And u1 checks that s1/file1 does not have extended attribute <name>

    Examples:
    | name | value |
    | user.xattr1 | WHATEVER |


  Scenario Outline: Check string extended attribute has correct value
    When u1 creates regular files [s1/file1]
    Then u1 sees file1 in s1
    Then u1 sets on s1/file1 extended attribute <name> with value <value>
    And u1 checks that s1/file1 has string extended attribute <name> with value <value>
    
    Examples:
    | name | value |
    | user.withspaces | EXTENDED ATTRIBUTE VALUE |
    | user.unicode | महसुस |
    | user.notjson | {{{{{ "a": 1 }} |


  Scenario Outline: Check numeric extended attribute has correct value
    When u1 creates regular files [s1/file1]
    Then u1 sees file1 in s1
    Then u1 sets on s1/file1 extended attribute <name> with value <value>
    And u1 checks that s1/file1 has numeric extended attribute <name> with value <value>
    
    Examples:
    | name | value |
    | user.int | 987654 |
    | user.negative | -123.9 |
    | user.zero | -0.00 |
    | user.exp | 6.0865000001e-03 |


  Scenario Outline: Check JSON extended attribute has correct value
    When u1 creates regular files [s1/file1]
    Then u1 sees file1 in s1
    Then u1 sets on s1/file1 extended attribute <name> with value <value>
    And u1 checks that s1/file1 has JSON extended attribute <name> with value <value>
    
    Examples:
    | name | value |
    | user.emptyobject | {} |
    | user.simpleobject | {"abcd": 4} |
    | user.list | [1, 2, 3, {"a": [1, 2, 3]}] |
