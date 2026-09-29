const { chromium } = require('playwright');

const testUrl = process.env.BROWSER_TEST_URL ?? 'http://127.0.0.1:8080';

async function run() {
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage();
    let rejectTest;
    const testResult = new Promise((resolve, reject) => {
      rejectTest = reject;
      page.on('console', message => {
        const text = message.text();
        if (text === 'SUCCESS') {
          resolve();
        } else if (text === 'ERROR') {
          reject(new Error('Browser client reported ERROR'));
        } else {
          console.log(text);
        }
      });
      page.on('pageerror', reject);
    });
    const timeout = setTimeout(() => rejectTest(new Error('Browser client did not finish')), 30000);
    try {
      await page.goto(testUrl);
      await testResult;
      console.log('SUCCESS');
    } finally {
      clearTimeout(timeout);
    }
  } finally {
    await browser.close();
  }
}

run().catch(error => {
  console.error(error);
  process.exitCode = 1;
});