module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'jsdom',
  setupFilesAfterEnv:['<rootDir>/src/__test__/config/importJestDOM.ts'],
  moduleNameMapper: {
    '\\.(css|scss)$': '<rootDir>/src/__test__/__mocks__/styleMock.js',
  },
  moduleDirectories: ["node_modules", "src"],
  transform: { "^.+\\.(ts|tsx|js|jsx)?$": "ts-jest" },
  transformIgnorePatterns: ['<rootDir>/node_modules/']

};