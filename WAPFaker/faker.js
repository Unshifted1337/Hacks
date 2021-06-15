const faker = require('faker');

exports.faker = function (options) {
  
  faker.locale = 'en'
  if (options.seed) faker.seed(options.seed)
  if (options.locale) faker.locale = options.locale
  return faker.fake(options.data);
};
