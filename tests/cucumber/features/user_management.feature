Feature: User management

  Background:
    Given openId server is started

  Scenario: User registration
    When u1 registers in onedata
    And u1 is registered